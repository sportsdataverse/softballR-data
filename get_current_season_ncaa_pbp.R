install.packages("glue")
install.packages("stringr")
install.packages("rvest")
install.packages("dplyr")
install.packages("tidyr")
install.packages("magrittr")

library(glue)
library(stringr)
library(rvest)
library(dplyr)
library(tidyr)
library(magrittr)

ncaa_softball_pbp <- function(game_id){

  options(warn = -1)

  box_raw <- glue::glue("https://stats.ncaa.org/contests/{game_id}/box_score") %>%
    readLines()

  pbp_id <- sub('.*/(\\d+).*', '\\1', box_raw[grep("Play by Play", box_raw)[1]])

  possible_events <- c("struck out swinging",
                       "struck out looking",
                       "grounded out",
                       "flied out",
                       "infield fly",
                       "hit into double play",
                       "lined out",
                       "out",
                       "reached on a fielder's choice",
                       "reached on an error",
                       "reached on a throwing error",
                       "reached on a fielding error",
                       "lined into double play",
                       "grounded into double play",
                       "hit by pitch",
                       "walked",
                       "intentionally walked",
                       "fouled out",
                       "fouled into double play",
                       "popped up",
                       "singled",
                       "doubled",
                       "tripled",
                       "homered")

  raw <- glue::glue("https://stats.ncaa.org/game/play_by_play/{pbp_id}") %>%
    rvest::read_html() %>%
    rvest::html_table()

  if(length(raw) == 0) return(NULL)
  
  upd <- raw[(5:length(raw))][c(F,T)]

  df <- do.call(rbind, upd)

  names(df) <- df[1,]

  team_name <- df[1,1] %>% as.character()
  team_col <- 1

  opponent_name <- df[1,3] %>% as.character()
  opponent_col <- 3
  
  filtered <- df %>%
    dplyr::rename(team = team_col, opponent = opponent_col) %>%
    tidyr::separate(Score, c("away_team_runs", "home_team_runs"), sep = "-") %>%
    dplyr::select(team, away_team_runs, opponent, home_team_runs) %>%
    dplyr::mutate(new_inning = team == team_name,
                  inning = cumsum(new_inning)) %>%
    dplyr::filter(stringr::str_detect(team, paste(possible_events, collapse = "|")) |
                    stringr::str_detect(opponent, paste(possible_events, collapse = "|")) |
                    stringr::str_detect(team, "\\(|R:") |
                    stringr::str_detect(opponent, "\\(|R:")) %>%
    dplyr::select(-new_inning)

  away_team <- ifelse(filtered$team[1] != "", team_col, opponent_col)
  top_bottom <- as.character(ifelse(!filtered[away_team] == "", "bottom", "top"))

  filtered <- filtered  %>%
    dplyr::mutate(top_bottom = top_bottom,
                  events = ifelse(team == "", opponent, team),
                  team = ifelse(team == "", opponent_name, team_name),
                  opponent = ifelse(opponent == "", opponent_name, team_name)) %>%
    dplyr::filter(!stringr::str_detect(events, "H: "))

  # Play ID Format: game_id, inning, top/bottom (top = 0, bottom = 1), play #

  pbp <- filtered %>%
    dplyr::mutate(top_bottom = top_bottom,
                  game_id = game_id) %>%
    dplyr::group_by(inning, top_bottom) %>%
    dplyr::mutate(play_id = paste0(game_id, "_", inning, "_", as.numeric(top_bottom == "bottom"), "_", dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::filter(away_team_runs != "Score")

  return(pbp)

}

load_ncaa_softball_scoreboard <- function(season, division = "D1"){

  if(!is.numeric(season)) return("Invalid Input")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid Division")

  if(min(season) < 2016 | max(season) > 2023) stop("Invalid Season")

  if(length(season) == 1){

    if(division == "D1"){

      url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{season}.RDS?raw=true")

    } else{

      url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{division}_{season}.RDS?raw=true")

    }

    con <- url(url)

    on.exit(close(con))

    scoreboard <- try(readRDS(con), silent = TRUE)

  } else{

    scoreboard <- data.frame()

    for(i in 1:length(season)){

      if(division == "D1"){

        url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{season[i]}.RDS?raw=true")

      } else{

        url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_{division}_{season[i]}.RDS?raw=true")

      }

      con <- url(url)

      on.exit(close(con))

      scoreboard <- rbind(scoreboard, try(readRDS(con), silent = TRUE))
    }

  }

  return(scoreboard)

}

load_ncaa_softball_pbp <- function(season, division = "D1"){

  if(!is.numeric(season)) stop("Invalid Input")

  if(season != 2023) stop("Invalid Season")

  if(!(division %in% c("D1", "D2", "D3"))) stop("Invalid division")

  division_id <- stringr::str_replace(division, "D", "d")

  url <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/{division_id}_ncaa_pbp_{season}.RDS?raw=true")

  con <- url(url)

  on.exit(close(con))

  scoreboard <- try(readRDS(con), silent = TRUE)

  return(scoreboard)

}

d1_scoreboard <- load_ncaa_softball_scoreboard(2023, division = "D1") %>% mutate(game_date = stringr::word(game_date, 1, 3, sep = "/| |<"))
d2_scoreboard <- load_ncaa_softball_scoreboard(2023, division = "D2") %>% mutate(game_date = stringr::word(game_date, 1, 3, sep = "/| |<"))
d3_scoreboard <- load_ncaa_softball_scoreboard(2023, division = "D3") %>% mutate(game_date = stringr::word(game_date, 1, 3, sep = "/| |<"))


d1_pbp <- load_ncaa_softball_pbp(2023, division = "D1")
d2_pbp <- load_ncaa_softball_pbp(2023, division = "D2")
d3_pbp <- load_ncaa_softball_pbp(2023, division = "D3")

most_recent <- max(c(d1_pbp$game_date, d2_pbp$game_date, d3_pbp$game_date))

d1_games <- d1_scoreboard %>%
  filter(game_date > most_recent)

d2_games <- d2_scoreboard %>%
  filter(game_date > most_recent)

d3_games <- d3_scoreboard %>%
  filter(game_date > most_recent)

if(nrow(d1_games > 0)){
  for(i in 1:nrow(d1_games)){
    
    curr_pbp <- ncaa_softball_pbp(d1_games$game_id[i])
    
    if(!(is.null(curr_pbp))){
     
      d1_pbp <- rbind(d1_pbp, curr_pbp %>%
                        mutate(game_date = d1_games$game_date[i]))
       
    }

  }

}

if(nrow(d2_games) > 0){
  for(i in 1:nrow(d2_games)){

    curr_pbp <- ncaa_softball_pbp(d2_games$game_id[i])
    
    if(!(is.null(curr_pbp))){
      
      d2_pbp <- rbind(d2_pbp, curr_pbp %>%
                        mutate(game_date = d2_games$game_date[i]))
      
    }
  }
}

if(nrow(d3_games) > 0){
  for(i in 1:nrow(d3_games)){

    curr_pbp <- ncaa_softball_pbp(d3_games$game_id[i])
    
    if(!(is.null(curr_pbp))){
      
      d3_pbp <- rbind(d3_pbp, curr_pbp %>%
                        mutate(game_date = d3_games$game_date[i]))
      
    }

  }

}

saveRDS(d1_pbp, "data/d1_ncaa_pbp_2023.RDS")
saveRDS(d2_pbp, "data/d2_ncaa_pbp_2023.RDS")
saveRDS(d3_pbp, "data/d3_ncaa_pbp_2023.RDS")
