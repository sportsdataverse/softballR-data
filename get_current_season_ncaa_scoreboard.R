install.packages("lubridate")
install.packages("dplyr")
install.packages("rvest")
install.packages("magrittr")
install.packages("stringr")

library(lubridate)
library(dplyr)
library(rvest)
library(magrittr)
library(stringr)

get_ncaa_scoreboard <- function(date){

  if(as.Date(date) >= Sys.Date()){
    stop("Invalid Date")
  }

  if(class(date) != "Date"){

    year <- try(strsplit(date, "-")[[1]][1])
    month <- try(strsplit(date, "-")[[1]][2])
    day <- try(strsplit(date, "-")[[1]][3])

  } else{

    month <- lubridate::month(date)
    day <- lubridate::day(date)
    year <- lubridate::year(date)

  }

  division_id <- dplyr::case_when(year == 2023 ~ 18101,
                                  year == 2022 ~ 17840,
                                  year == 2021 ~ 15620,
                                  year == 2020 ~ 15220,
                                  year == 2019 ~ 16820)


  test <- paste0("https://stats.ncaa.org/season_divisions/",division_id,"/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=",month,"%2F",day,"%2F",year) %>%
    rvest::read_html() %>%
    rvest::html_text() %>%
    strsplit("Box Score") %>%
    magrittr::extract2(1) %>%
    strsplit("\\n")

  for(i in 1:length(test)){

    for(j in 1:length(test[[i]])){

      test[[i]][j] <- test[[i]][j] %>% trimws()

    }

    test[[i]] <- test[[i]][nzchar(test[[i]])]

  }

  start_loc <- grep("Attendance",test[[1]])

  test[[1]] <- test[[1]][(start_loc+1):length(test[[1]])]

  assemble_df <- function(game_vector){

    if("Canceled" %in% game_vector){

      num_canceled <- length(grep("Canceled",game_vector))

      date_locs <- grep(paste(month,day,year,sep = "/"),game_vector)

      game_vector <- game_vector[-c(1:(date_locs[num_canceled+1]-1))]
    }

    date <- game_vector[1] %>% stringr::str_remove_all(" \\(1\\)| \\(2\\)")

    game_vector <- game_vector[!stringr::str_detect(game_vector, " \\(1\\)| \\(2\\)")]

    team1 <- game_vector[grep("\\(",game_vector)[1]] %>% strsplit(" \\(") %>% magrittr::extract2(1) %>% magrittr::extract(1)
    team2 <- game_vector[grep("\\(",game_vector)[2]] %>% strsplit(" \\(") %>% magrittr::extract2(1) %>% magrittr::extract(1)

    team1_runs <- game_vector[grep("Final",game_vector) - 1] %>% as.numeric
    team2_runs <- game_vector[length(game_vector)] %>% as.numeric

    upd_game_vector <- game_vector[!(game_vector) %in%
                                     c(date, team1, team2, team1_runs, team2_runs, "Final")]

    upd_game_vector <- upd_game_vector[-c(length(upd_game_vector), length(upd_game_vector) - 1)]

    game_df <- data.frame(date, team1, team2, team1_runs, team2_runs)

    return(game_df)

  }

  games_df <- data.frame()

  for(i in 1:(length(test)-1)){

    games_df <- rbind(games_df, assemble_df(test[[i]]))

  }

  return(games_df)

}

get_ncaa_season_scoreboard <- function(season){
  options(warn = -1)

  s <- try(as.numeric(season))

  if("try-error" %in% class(s) || is.na(s) || s < 2022 || s > 2023){
    stop("Invalid Season")
  }

  seasons <- data.frame(season = 2015:2023,
                        start_date = c("2015-02-05","2016-02-11","2017-02-09","2018-02-08","2019-02-07","2020-02-06","2021-02-11","2022-02-10","2023-02-09"),
                        end_date = c("2015-06-03","2016-06-08","2017-06-07","2018-06-06","2019-06-04","2020-03-12","2021-06-10","2022-06-09","2023-06-09")) #Go back and fix after season

  start_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(start_date) %>% as.character() %>% as.Date()
  end_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(end_date) %>% as.character() %>% as.Date()

  scoreboard <- data.frame()

  for(i in seq(start_date,min(end_date,Sys.Date()),1)){
    date = as.character(as.Date(i,origin = "1970-01-01"))

    temp <- try(get_ncaa_scoreboard(date),silent = TRUE)

    if("try-error" %in% class(temp)){
      next
    }

    scoreboard <- rbind(scoreboard,temp)
  }

  return(scoreboard)
}

scoreboard <- get_ncaa_season_scoreboard(2023)

saveRDS(object = scoreboard, file = "data/ncaa_scoreboard_2023.RDS")
