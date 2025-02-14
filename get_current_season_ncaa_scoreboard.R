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

ncaa_softball_scoreboard <- function(date, division = "D1"){
  
  if(as.Date(date) >= Sys.Date()){
    stop("Invalid Date")
  }
  
  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")
  
  if(!(is(date, "Date"))){
    
    year <- try(strsplit(date, "-")[[1]][1])
    month <- try(strsplit(date, "-")[[1]][2])
    day <- try(strsplit(date, "-")[[1]][3])
    
  } else{
    
    month <- lubridate::month(date)
    day <- lubridate::day(date)
    year <- lubridate::year(date)
    
  }
  
  division_id <- dplyr::case_when(division == "D1" & year == 2025 ~ 18503,
                                  division == "D1" & year == 2024 ~ 18261,
                                  division == "D1" & year == 2023 ~ 18101,
                                  division == "D1" & year == 2022 ~ 17840,
                                  division == "D1" & year == 2021 ~ 17540,
                                  division == "D1" & year == 2020 ~ 17103,
                                  division == "D1" & year == 2019 ~ 16820,
                                  division == "D1" & year == 2018 ~ 15196,
                                  division == "D1" & year == 2017 ~ 13300,
                                  division == "D1" & year == 2016 ~ 12981,
                                  division == "D2" & year == 2025 ~ 18504,
                                  division == "D2" & year == 2024 ~ 18264,
                                  division == "D2" & year == 2023 ~ 18102,
                                  division == "D2" & year == 2022 ~ 17841,
                                  division == "D2" & year == 2021 ~ 17541,
                                  division == "D2" & year == 2020 ~ 17104,
                                  division == "D2" & year == 2019 ~ 16821,
                                  division == "D2" & year == 2018 ~ 15197,
                                  division == "D2" & year == 2017 ~ 13301,
                                  division == "D2" & year == 2016 ~ 12980,
                                  division == "D3" & year == 2025 ~ 18505,
                                  division == "D3" & year == 2024 ~ 18265,
                                  division == "D3" & year == 2023 ~ 18103,
                                  division == "D3" & year == 2022 ~ 17842,
                                  division == "D3" & year == 2021 ~ 17542,
                                  division == "D3" & year == 2020 ~ 17105,
                                  division == "D3" & year == 2019 ~ 16822,
                                  division == "D3" & year == 2018 ~ 15198,
                                  division == "D3" & year == 2017 ~ 13302,
                                  division == "D3" & year == 2016 ~ 12982)
  
  
  raw <- glue::glue("https://stats.ncaa.org/season_divisions/{division_id}/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date={month}%2F{day}%2F{year}&conference_id=0&tournament_id=&commit=Submit") %>%
    readLines()
  
  locs <- grep("<tr id=\"", raw)
  
  if(length(locs) == 0) return(NULL)
  
  assemble_df <- function(loc, next_loc){
    
    game_vec <- raw[loc:(next_loc-1)]
    
    if(any(grepl("Canceled|Ppd", game_vec))) return(NULL)
    
    game_id <- game_vec[grep("<tr id=\"", game_vec)[1]] %>%
      trimws() %>%
      stringr::str_remove_all("<tr id=\"contest_|\">")
    
    game_date <- game_vec[grep("<td rowspan=\"2\" valign=\"middle\">", game_vec)[1] + 1] %>%
      trimws()
    
    away_team <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[1]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)
    
    away_team_id <- game_vec[grep("<a target=\"TEAMS_WIN\" class=\"skipMask\" href=\"/teams/",game_vec)[1]] %>%
      strsplit("href=\"/teams/|\">") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)
    
    away_team_logo <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[1]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(3) %>%
      stringr::str_remove_all("\" />")
    
    away_team_runs <- game_vec[grep("<div id=\"score_", game_vec)[1] + 1] %>%
      trimws()
    
    home_team <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[2]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)
    
    home_team_id <- game_vec[grep("<a target=\"TEAMS_WIN\" class=\"skipMask\" href=\"/teams/",game_vec)[2]] %>%
      strsplit("href=\"/teams/|\">") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(2)
    
    home_team_logo <- game_vec[grep("<img height=\"20px\" width=\"30px\" alt=\"",game_vec)[2]] %>%
      strsplit("alt=\"|\" src=\"") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(3) %>%
      stringr::str_remove_all("\" />")
    
    home_team_runs <- game_vec[grep("<div id=\"score_", game_vec)[2] + 1] %>%
      trimws()
    
    if(is.na(home_team_runs)){
      print(game_vec)
      print(grep("<div id=\"score_", game_vec))
    }
    
    status <- game_vec[grep("<div class=\"livestream", game_vec) + 1] %>%
      trimws()
    
    game_df <- data.frame(away_team, away_team_id, away_team_logo, away_team_runs,
                          home_team, home_team_id, home_team_logo, home_team_runs,
                          game_date, game_id, status) %>%
      filter(status == "Final")
    
    return(game_df)
    
  }
  
  games_df <- data.frame()
  
  for(i in 1:(length(locs) - 1)){
    
    if(i %% 2 == 0) next
    
    loc <- locs[i]
    
    if(i == length(locs) - 3){
      next_loc <- length(raw)
    } else{
      next_loc <- locs[i + 2]
    }
    
    games_df <- rbind(games_df, assemble_df(loc, loc + 75))
    
  }
  
  if(length(games_df)== 0) return(NULL)
  
  games_df <- games_df %>%
    dplyr::filter(away_team_runs != "") %>%
    dplyr::mutate(home_team_runs = as.numeric(home_team_runs),
                  away_team_runs = as.numeric(away_team_runs),
                  game_date = stringr::str_remove_all(game_date, " \\(1\\)| \\(2\\)| \\<br/\\>\\*If necessary")) %>%
    dplyr::distinct()
  
  print(paste0(date, ": completed"))
  
  return(games_df)
  
}

ncaa_softball_season_scoreboard <- function(season, division = "D1"){
  options(warn = -1)

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  s <- try(as.numeric(season))

  if("try-error" %in% class(s) || is.na(s) || s < 2017 || s > 2025){
    stop("Invalid Season")
  }

  seasons <- data.frame(season = 2015:2025,
                        start_date = c("2015-02-05","2016-02-11","2017-02-09","2018-02-08","2019-02-07","2020-02-06","2021-02-11","2022-02-10","2023-02-09", "2024-02-08", "2025-02-04"),
                        end_date = c("2015-06-03","2016-06-08","2017-06-07","2018-06-06","2019-06-04","2020-03-12","2021-06-10","2022-06-09","2023-06-09", "2024-06-06", "2025-06-09")) #Go back and fix after season

  start_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(start_date) %>% as.character() %>% as.Date()
  end_date <- seasons %>% dplyr::filter(season == s) %>% dplyr::select(end_date) %>% as.character() %>% as.Date()

  scoreboard <- data.frame()

  dates <- seq(start_date,min(end_date,Sys.Date()-1),1)

  scoreboard <- do.call(rbind, lapply(X = dates, FUN = ncaa_softball_scoreboard, division = division)) %>%
    dplyr::distinct()

  return(scoreboard)
}


scoreboard_d1 <- try(ncaa_softball_season_scoreboard(season = 2025, division = "D1"))

scoreboard_d2 <- try(ncaa_softball_season_scoreboard(season = 2025, division = "D2"))

scoreboard_d3 <- try(ncaa_softball_season_scoreboard(season = 2025, division = "D3"))

if(!("try-error" %in% class(scoreboard_d1))){
  saveRDS(object = scoreboard_d1, file = "data/ncaa_scoreboard_2025.RDS")
}
if(!("try-error" %in% class(scoreboard_d2))){
  saveRDS(object = scoreboard_d2, file = "data/ncaa_scoreboard_D2_2025.RDS")
}
if(!("try-error" %in% class(scoreboard_d3))){
  saveRDS(object = scoreboard_d3, file = "data/ncaa_scoreboard_D3_2025.RDS")
}
