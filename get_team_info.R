library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)

team_ids <- readRDS(url("https://github.com/sportsdataverse/softballR-data/raw/main/data/ncaa_team_info.RDS")) %>% 
  select(team_id, team_name) %>% 
  distinct()

team_info <- data.frame()

for(i in 1:nrow(team_ids)){
  
  print(i)
  
  url <- paste0("https://stats.ncaa.org/teams/history/WSB/", team_ids$team_id[i])
  
  table <- url %>% 
    read_html() %>% 
    html_table() %>% 
    extract2(1) %>% 
    clean_names() %>% 
    mutate(season = paste0(substr(year, 1, 2), substr(year, 6, 7)),
           team_name = team_ids$team_name[i],
           team_id = team_ids$team_id[i]) %>%
    rename(head_coach = head_coaches,
           win_perc = wl_percent) %>% 
    select(team_name, team_id, season, head_coach, division, conference, wins, losses, ties, win_perc)
  
  team_info <- rbind(team_info, table)
  
}

saveRDS(team_info, "~/Projects/softballR-data/data/ncaa_team_info.RDS")
