install.packages("tidyverse")
library(tidyverse)

install.packages("janitor")
library(janitor)

install.packages("rvest")
library(rvest)

install.packages("anytime")
library(anytime)

install.packages("glue")
library(glue)

options(warn = -1)

get_pitching_box <- function(id){

  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/individual_stats") %>%
    rvest::read_html() %>%
    rvest::html_table()
  
  if(length(raw) == 0){
    return(data.frame())
  }

  first_team <- as.character(raw[[2]][2,1])
  second_team <- as.character(raw[[2]][3,1])
  date <- as.character(raw[[2]][4,1])

  upd <- rbind(raw[[6]],raw[[7]]) %>%
    janitor::clean_names() %>%
    dplyr::rename(player = name) %>%
    dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

  upd$team <- ifelse(upd$player %in% raw[[6]]$Name, first_team, second_team)
  upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
  upd[upd == ""] <- "0"
  upd[] <- lapply(upd, gsub, pattern="/", replacement="")

  cols = c("game_id", "team", "opponent", "player", "ip", "ha", "er", "bb", "hb", "so", "bf", "hr_a", "season")

  upd <- upd %>%
    dplyr::mutate(game_id = id, season = 2024) %>%
    dplyr::select(cols) %>%
    dplyr::mutate(across(5:12, as.numeric)) %>%
    dplyr::filter(ip > 0)

  return(upd)

}

get_hitting_box <- function(id){

  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/individual_stats") %>%
    rvest::read_html() %>%
    rvest::html_table()
  
  if(length(raw) == 0){
    return(data.frame())
  }
  
  first_team <- as.character(raw[[2]][2,1])
  second_team <- as.character(raw[[2]][3,1])
  date <- as.character(raw[[2]][4,1])

  upd <- rbind(raw[[4]],raw[[5]]) %>%
    janitor::clean_names() %>%
    dplyr::rename(player = name) %>%
    dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

  upd$team <- ifelse(upd$player %in% raw[[4]]$Name, first_team, second_team)
  upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
  upd[upd == ""] <- "0"

  cols = c("player", "pos", "g", "rbi", "ab", "r", "h", "x2b", "x3b", "tb", "hr", "ibb", "bb", "hbp", "sf", "sh", "k", "dp", "sb", "cs", "picked", "team", "opponent", "game_id", "game_date", "season")

  upd <- upd %>%
    dplyr::rename(pos = p) %>%
    dplyr::mutate(g = 1, game_date = date, season = 2024, game_id = id) %>%
    dplyr::select(cols) %>%
    dplyr::mutate(across(.cols = 3:20, .fns = \(col) as.numeric(str_remove(col, "/"))))

  return(upd)

}


url_d1 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_2025.RDS?raw=true")
url_d2 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D2_2025.RDS?raw=true")
url_d3 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D3_2025.RDS?raw=true")

url_d1 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_2025.RDS?raw=true")
url_d2 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D2_2025.RDS?raw=true")
url_d3 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D3_2025.RDS?raw=true")


con <- url(url_d1)

on.exit(close(con))

scoreboard_d1 <- try(readRDS(con), silent = TRUE) %>%
  distinct(game_id, game_date)

con <- url(url_d2)

on.exit(close(con))

scoreboard_d2 <- try(readRDS(con), silent = TRUE) %>%
  distinct(game_id, game_date)

con <- url(url_d3)

on.exit(close(con))

scoreboard_d3 <- try(readRDS(con), silent = TRUE) %>%
  distinct(game_id, game_date)

most_recent_d1 <- try(max(anydate(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d1_hitting_box_scores_2025.RDS")) %>% pull(game_date))))
most_recent_d2 <- try(max(anydate(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d2_hitting_box_scores_2025.RDS")) %>% pull(game_date))))
most_recent_d3 <- try(max(anydate(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d3_hitting_box_scores_2025.RDS")) %>% pull(game_date))))

if("try-error" %in% class(most_recent_d1)){
  most_recent_d1 <- "1900-01-01"
  most_recent_d2 <- "1900-01-01"
  most_recent_d3 <- "1900-01-01"
}

d1_hitting_box <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d1_hitting_box_scores_2025.RDS"))
d2_hitting_box <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d2_hitting_box_scores_2025.RDS"))
d3_hitting_box <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d3_hitting_box_scores_2025.RDS"))

scoreboard_d1$in_box <- scoreboard_d1$game_id %in% d1_hitting_box$game_id
scoreboard_d2$in_box <- scoreboard_d2$game_id %in% d2_hitting_box$game_id
scoreboard_d3$in_box <- scoreboard_d3$game_id %in% d3_hitting_box$game_id

game_ids_d1 <- scoreboard_d1 %>% filter(anydate(game_date) > most_recent_d1 | !in_box) %>% pull(game_id) %>% sort
game_ids_d2 <- scoreboard_d2 %>% filter(anydate(game_date) > most_recent_d2) %>% pull(game_id) %>% sort
game_ids_d3 <- scoreboard_d3 %>% filter(anydate(game_date) > most_recent_d3) %>% pull(game_id) %>% sort

get_ncaa_hitter_player_box <- function(game_id){

  print(i)

  i <<- i + 1

  hitting <- try(get_hitting_box(game_id))

  return(hitting)

}

get_ncaa_pitcher_player_box <- function(game_id){

  print(i)

  i <<- i + 1

  pitching <- try(get_pitching_box(game_id))

  return(pitching)

}

# Hitter box scores

i <- 0

prev_box <- d1_hitting_box
box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_hitter_player_box))
hitting_cols <- c("player", "pos", "g", "rbi", "ab", "r", "h", "x2b", "x3b", "tb", "hr", "ibb", "bb", "hbp", "sf", "sh", "k", "dp", "sb", "cs", "picked", "team", "opponent", "game_id", "game_date", "season")

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = c("game_id", "game_date")) %>%
    mutate(season = 2025) %>%
    select(hitting_cols) %>%
    mutate(across(3:20, as.numeric))

  saveRDS(object = rbind(prev_box, box), file = "data/d1_hitting_box_scores_2025.RDS")
}


i <- 0

prev_box <- d2_hitting_box
box <- do.call(rbind, lapply(X = game_ids_d2, FUN = get_ncaa_hitter_player_box))

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d2, by = c("game_id", "game_date")) %>%
    mutate(season = 2025) %>%
    select(hitting_cols) %>%
    mutate(across(3:20, as.numeric))

  saveRDS(object = rbind(prev_box, box), file = "data/d2_hitting_box_scores_2025.RDS")
}

i <- 0

prev_box <- d3_hitting_box
box <- do.call(rbind, lapply(X = game_ids_d3, FUN = get_ncaa_hitter_player_box))

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d3, by = c("game_id", "game_date")) %>%
    mutate(season = 2025) %>%
    select(hitting_cols) %>%
    mutate(across(3:20, as.numeric))

  saveRDS(object = rbind(prev_box, box), file = "data/d3_hitting_box_scores_2025.RDS")
}

# Pitcher box scores

d1_pitching_box <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d1_pitching_box_scores_2025.RDS"))
d2_pitching_box <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d2_pitching_box_scores_2025.RDS"))
d3_pitching_box <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d3_pitching_box_scores_2025.RDS"))

scoreboard_d1$in_box <- scoreboard_d1$game_id %in% d1_pitching_box$game_id
scoreboard_d2$in_box <- scoreboard_d2$game_id %in% d2_pitching_box$game_id
scoreboard_d3$in_box <- scoreboard_d3$game_id %in% d3_pitching_box$game_id

game_ids_d1 <- scoreboard_d1 %>% filter(anydate(game_date) > most_recent_d1 | !in_box) %>% pull(game_id) %>% sort
game_ids_d2 <- scoreboard_d2 %>% filter(anydate(game_date) > most_recent_d2 | !in_box) %>% pull(game_id) %>% sort
game_ids_d3 <- scoreboard_d3 %>% filter(anydate(game_date) > most_recent_d3 | !in_box) %>% pull(game_id) %>% sort

i <- 0

prev_box <- d1_pitching_box
box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_pitcher_player_box))
pitching_cols <- c("team", "opponent", "player", "ip", "ha", "er", "bb", "hb", "so", "bf", "hr_a", "game_id", "game_date", "season")

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = "game_id") %>%
    mutate(season = 2025) %>%
    select(pitching_cols) %>%
    mutate(across(5:12, as.numeric))

  saveRDS(object = rbind(prev_box, box), file = "data/d1_pitching_box_scores_2025.RDS")

}

i <- 0

prev_box <- d2_pitching_box
box <- do.call(rbind, lapply(X = game_ids_d2, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d2, by = "game_id") %>%
    mutate(season = 2025) %>%
    select(pitching_cols) %>%
    mutate(across(5:12, as.numeric))


  saveRDS(object = rbind(prev_box, box), file = "data/d2_pitching_box_scores_2025.RDS")
}



i <- 0

prev_box <- d3_pitching_box
box <- do.call(rbind, lapply(X = game_ids_d3, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d3, by = "game_id") %>%
    mutate(season = 2025) %>%
    select(pitching_cols) %>%
    mutate(across(5:12, as.numeric))

  saveRDS(object = rbind(prev_box, box), file = "data/d3_pitching_box_scores_2025.RDS")
}

