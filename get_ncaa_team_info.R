library(rvest)
library(magrittr)
library(glue)
library(httr)

team_ids <- "https://stats.ncaa.org/game_upload/team_codes" %>%
  rvest::read_html() %>%
  rvest::html_table() %>%
  magrittr::extract2(1) %>%
  dplyr::filter(!(X1 %in% c("NCAA Codes", "ID"))) %>%
  `names<-`(c("team_id", "team_name"))

team_info <- data.frame()

for(i in 1:nrow(team_ids)){
  
  url <- glue("https://stats.ncaa.org/teams/history/WSB/{team_ids[i, 'team_id']}")
  
  response <- try(url %>% read_html() %>% html_table())
  
  if("try-error" %in% class(response)){print("failed, trying again"); response <- try(url %>% read_html() %>% html_table())}
  
  if("try-error" %in% class(response)){print("failed twice, trying again"); response <- try(url %>% read_html() %>% html_table())}
  
  if("try-error" %in% class(response)){print(glue("failed three times, giving up on {team_ids$team_name[i]}")); next}
  
  temp <- response %>% 
    extract2(1) %>% 
    filter(Year != "2023-24") %>% 
    mutate(team_id = as.numeric(team_ids[i, 'team_id']),
           team_name = as.character(team_ids[i, 'team_name'])) %>% 
    select(-Notes) %>% 
    `names<-`(c("season", "head_coach", "division", "conference", "wins", "losses", "ties", "win_perc", "team_id", "team_name")) %>% 
    select(team_name, team_id, season, head_coach, division, conference, wins, losses, ties, win_perc) %>% 
    mutate(across(.cols = c(wins, losses, ties),
                  .fns = \(col) ifelse(is.na(col), 0, col)),
           season = ifelse(season == "1999-00", 2000, as.numeric(paste0(substr(season, 1, 2), substr(season, 6, 7))))) %>% 
    drop_na(season)
  
  team_info <- rbind(team_info, temp)
  
  if(i %% 10 == 0) print(i)
}

saveRDS(team_info, file = "~/Projects/softballR-data/data/ncaa_team_info.RDS")


