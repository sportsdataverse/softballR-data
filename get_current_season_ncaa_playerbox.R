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

  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
    readLines()

  pitching_id <- raw[grep("\t   <a href=\"/game/box_score/", raw)] %>%
    stringr::str_remove_all("\t   <a href=\"/game/box_score/|\">Box Score </a>")

  raw <- glue::glue("https://stats.ncaa.org/game/box_score/{pitching_id}?year_stat_category_id=15021") %>%
    rvest::read_html() %>%
    rvest::html_table()

  first_team <- as.character(raw[[6]][1,1])
  second_team <- as.character(raw[[7]][1,1])

  upd <- rbind(raw[[6]],raw[[7]]) %>%
    `names<-`(raw[[6]][2,]) %>%
    janitor::clean_names() %>%
    dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

  upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
  upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
  upd[upd == ""] <- "0"
  upd[] <- lapply(upd, gsub, pattern="/", replacement="")

  upd <- upd %>%
    dplyr::mutate(across(3:26, as.numeric)) %>%
    dplyr::filter(ip > 0) %>%
    dplyr::mutate(game_id = id)

  return(upd)

}

get_hitting_box <- function(id){

  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
    rvest::read_html() %>%
    rvest::html_table()

  first_team <- as.character(raw[[6]][1,1])
  second_team <- as.character(raw[[7]][1,1])

  upd <- rbind(raw[[6]],raw[[7]]) %>%
    `names<-`(raw[[6]][2,]) %>%
    janitor::clean_names() %>%
    dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))

  upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
  upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
  upd[upd == ""] <- "0"

  upd <- upd %>%
    dplyr::mutate(across(3:26, as.numeric)) %>%
    dplyr::mutate(game_id = id)

  return(upd)

}

get_fielding_box <- function(id){

  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
    readLines()

  fielding_id <- raw[grep("\t   <a href=\"/game/box_score/", raw)] %>%
    stringr::str_remove_all("\t   <a href=\"/game/box_score/|\">Box Score </a>")

  raw <- glue::glue("https://stats.ncaa.org/game/box_score/{fielding_id}?year_stat_category_id=15022") %>%
    rvest::read_html() %>%
    rvest::html_table()

  first_team <- as.character(raw[[6]][1,1])
  second_team <- as.character(raw[[7]][1,1])

  upd <- rbind(raw[[6]],raw[[7]]) %>%
    `names<-`(raw[[6]][2,]) %>%
    dplyr::filter(!(Player %in% c(first_team, second_team,"Player","Totals")))

  upd$team <- ifelse(upd$Player %in% raw[[6]]$X1, first_team, second_team)
  upd[upd == ""] <- "0"
  upd[] <- lapply(upd, gsub, pattern="/", replacement="")

  upd <- upd %>%
    dplyr::mutate(across(3:12, as.numeric)) %>%
    dplyr::mutate(game_id = id)

  return(upd)

}

curr_hitting_box_d1 <- readRDS("data/D1_hitting_box_scores_2023.RDS")
curr_pitching_box_d1 <- readRDS("data/D1_pitching_box_scores_2023.RDS")
curr_fielding_box_d1 <- readRDS("data/D1_fielding_box_scores_2023.RDS")

curr_hitting_box_d2 <- readRDS("data/D2_hitting_box_scores_2023.RDS")
curr_pitching_box_d2 <- readRDS("data/D2_pitching_box_scores_2023.RDS")

curr_hitting_box_d3 <- readRDS("data/D3_hitting_box_scores_2023.RDS")
curr_pitching_box_d3 <- readRDS("data/D3_pitching_box_scores_2023.RDS")


url_d1 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_2023.RDS?raw=true")
url_d2 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D2_2023.RDS?raw=true")
url_d3 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D3_2023.RDS?raw=true")


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


most_recent_d1 <- min(c(max(anydate(curr_hitting_box_d1$game_date)),
                     max(anydate(curr_pitching_box_d1$game_date))))

most_recent_d2 <- min(c(max(anydate(curr_hitting_box_d2$game_date)),
                        max(anydate(curr_pitching_box_d2$game_date))))


most_recent_d3 <- min(c(max(anydate(curr_hitting_box_d3$game_date)),
                        max(anydate(curr_pitching_box_d3$game_date))))


game_ids_d1 <- scoreboard_d1 %>% filter(anydate(game_date) > most_recent_d1) %>%  pull(game_id) %>% sort
game_ids_d2 <- scoreboard_d2 %>% filter(anydate(game_date) > most_recent_d2) %>%  pull(game_id) %>% sort
game_ids_d3 <- scoreboard_d3 %>% filter(anydate(game_date) > most_recent_d3) %>%  pull(game_id) %>% sort

get_ncaa_hitter_player_box <- function(game_id){

  i <<- i + 1

  hitting <- try(get_hitting_box(game_id))

  return(hitting)

}

get_ncaa_pitcher_player_box <- function(game_id){

  i <<- i + 1

  pitching <- try(get_pitching_box(game_id))

  return(pitching)

}

get_ncaa_fielding_player_box <- function(game_id){

  i <<- i + 1

  fielding <- try(get_fielding_box(game_id))

  return(fielding)

}

# Fielding box scores

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_fielding_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(Player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = "game_id")
  
  fielding_box <- rbind(curr_fielding_box_d1, box) %>%
    distinct()
  
  saveRDS(object = fielding_box, file = "data/d1_fielding_box_scores_2023.RDS")
}

# Hitter box scores

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_hitter_player_box))


if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = "game_id")
  
  hitting_box_d1 <- rbind(curr_hitting_box_d1, box) %>%
    distinct()
  
  saveRDS(object = hitting_box_d1, file = "data/D1_hitting_box_scores_2023.RDS")
}


i <- 0

box <- do.call(rbind, lapply(X = game_ids_d2, FUN = get_ncaa_hitter_player_box))



if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d2, by = "game_id")
  
  hitting_box_d2 <- rbind(curr_hitting_box_d2, box) %>%
    distinct()
  
  saveRDS(object = hitting_box_d2, file = "data/D2_hitting_box_scores_2023.RDS")
}

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d3, FUN = get_ncaa_hitter_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d3, by = "game_id")
  
  hitting_box_d3 <- rbind(curr_hitting_box_d3, box) %>%
    distinct()
  
  saveRDS(object = hitting_box_d3, file = "data/D3_hitting_box_scores_2023.RDS")
}

# Pitcher box scores

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = "game_id")
  
  pitching_box_d1 <- rbind(curr_pitching_box_d1, box) %>%
    distinct()
  
  saveRDS(object = pitching_box_d1, file = "data/D1_pitching_box_scores_2023.RDS")

}

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d2, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d2, by = "game_id")
  
  pitching_box_d2 <- rbind(curr_pitching_box_d2, box) %>%
    distinct()
  
  saveRDS(object = pitching_box_d2, file = "data/D2_pitching_box_scores_2023.RDS")
}



i <- 0

box <- do.call(rbind, lapply(X = game_ids_d3, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d3, by = "game_id")
  
  pitching_box_d3 <- rbind(curr_pitching_box_d3, box) %>%
    distinct()
  
  saveRDS(object = pitching_box_d3, file = "data/D3_pitching_box_scores_2023.RDS")
}
