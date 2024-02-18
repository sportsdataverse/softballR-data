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
  
  pitching_url <- raw[grep("Pitching", raw)] %>% 
    trimws() %>% 
    stringr::str_remove_all("\\<a href=\"|\"\\>Pitching\\</a\\>  &nbsp;\\|") %>% 
    paste0("https://stats.ncaa.org/", .)
  
  raw <- pitching_url %>%
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
    dplyr::mutate(across(3:35, as.numeric)) %>%
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
    dplyr::mutate(across(.cols = 3:76, .fns = \(col) as.numeric(str_remove(col, "/")))) %>%
    dplyr::mutate(game_id = id)
  
  return(upd)
  
}


url_d1 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_2024.RDS?raw=true")
url_d2 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D2_2024.RDS?raw=true")
url_d3 <- glue::glue("https://github.com/tmking2002/softballR-data/blob/main/data/ncaa_scoreboard_D3_2024.RDS?raw=true")


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

most_recent_d1 <- max(anydate(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d1_hitting_box_scores_2024.RDS")) %>% pull(game_date)))
most_recent_d2 <- max(anydate(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d2_hitting_box_scores_2024.RDS")) %>% pull(game_date)))
most_recent_d3 <- max(anydate(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d3_hitting_box_scores_2024.RDS")) %>% pull(game_date)))

game_ids_d1 <- scoreboard_d1 %>% filter(anydate(game_date) > most_recent_d1) %>% pull(game_id) %>% sort
game_ids_d2 <- scoreboard_d2 %>% filter(anydate(game_date) > most_recent_d2) %>% pull(game_id) %>% sort
game_ids_d3 <- scoreboard_d3 %>% filter(anydate(game_date) > most_recent_d3) %>% pull(game_id) %>% sort

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

# Hitter box scores

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_hitter_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = "game_id") %>% 
    mutate(season = 2024) %>% 
    select(-bb) %>% 
    rename(bb = bb_2) %>% 
    select(player, pos, g, rbi, ab, r, h, x2b, x3b, tb, hr, ibb, bb, hbp, sf, sh, k, kl, dp, gdp, tp, sb, cs, picked, go, fo, team, opponent, game_id, game_date, season) %>% 
    mutate(across(3:26, as.numeric))
  
  saveRDS(object = rbind(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d1_hitting_box_scores_2024.RDS")), box), file = "data/d1_hitting_box_scores_2024.RDS")
}


i <- 0

box <- do.call(rbind, lapply(X = game_ids_d2, FUN = get_ncaa_hitter_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d2, by = "game_id") %>% 
    mutate(season = 2024) %>% 
    select(-bb) %>% 
    rename(bb = bb_2) %>% 
    select(player, pos, g, rbi, ab, r, h, x2b, x3b, tb, hr, ibb, bb, hbp, sf, sh, k, kl, dp, gdp, tp, sb, cs, picked, go, fo, team, opponent, game_id, game_date, season) %>% 
    mutate(across(3:26, as.numeric))  

  saveRDS(object = rbind(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d2_hitting_box_scores_2024.RDS")), box), file = "data/d2_hitting_box_scores_2024.RDS")
}

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d3, FUN = get_ncaa_hitter_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d3, by = "game_id") %>% 
    mutate(season = 2024) %>% 
    select(player, pos, g, rbi, ab, r, h, x2b, x3b, tb, hr, ibb, bb, hbp, sf, sh, k, kl, dp, gdp, tp, sb, cs, picked, go, fo, team, opponent, game_id, game_date, season) %>% 
    mutate(across(3:26, as.numeric))  
  
  saveRDS(object = rbind(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d3_hitting_box_scores_2024.RDS")), box), file = "data/d3_hitting_box_scores_2024.RDS")
}

# Pitcher box scores

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d1, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){

  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d1, by = "game_id") %>% 
    mutate(season = 2024) %>% 
    select(game_id, team, opponent, player, ip, ha, er, bb, hb, so, bf, hr_a, go, fo, season) %>% 
    mutate(across(5:14, as.numeric))
  
  saveRDS(object = rbind(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d1_pitching_box_scores_2024.RDS")), box), file = "data/d1_pitching_box_scores_2024.RDS")
  
}

i <- 0

box <- do.call(rbind, lapply(X = game_ids_d2, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){
  
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d2, by = "game_id") %>% 
    mutate(season = 2024) %>% 
    select(game_id, team, opponent, player, ip, ha, er, bb, hb, so, bf, hr_a, go, fo, season) %>% 
    mutate(across(5:14, as.numeric))
  
  
  saveRDS(object = rbind(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d2_pitching_box_scores_2024.RDS")), box), file = "data/d2_pitching_box_scores_2024.RDS")
}



i <- 0

box <- do.call(rbind, lapply(X = game_ids_d3, FUN = get_ncaa_pitcher_player_box))

if(!(is.null(box))){
  box <- box %>%
    filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>%
    merge(scoreboard_d3, by = "game_id") %>% 
    mutate(season = 2024) %>% 
    select(game_id, team, opponent, player, ip, ha, er, bb, hb, so, bf, hr_a, go, fo, season) %>% 
    mutate(across(5:14, as.numeric))
  
  saveRDS(object = rbind(readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/d3_pitching_box_scores_2024.RDS")), box), file = "data/d3_pitching_box_scores_2024.RDS")
}
