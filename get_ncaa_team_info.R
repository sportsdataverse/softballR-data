team_ids <- "https://stats.ncaa.org/game_upload/team_codes" %>%
  rvest::read_html() %>%
  rvest::html_table() %>%
  magrittr::extract2(1) %>%
  dplyr::filter(!(X1 %in% c("NCAA Codes", "ID"))) %>%
  `names<-`(c("team_id", "team_name"))

get_info <- function(team_id){

  url <- glue::glue("https://stats.ncaa.org/teams/history/WSB/{team_id}")

  df <- url %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    dplyr::mutate(season = paste0(substr(Year,1,2),substr(Year,6,7))) %>%
    dplyr::select(season, `Head Coaches`, Division, Conference, Wins, Losses, Ties, `WL%`) %>%
    `names<-`(c("season","head_coach","division","conference","wins","losses","ties","win_perc")) %>%
    dplyr::mutate(team_id = team_id) %>%
    dplyr::filter(season != "")

}

info <- data.frame()

for(i in 1:nrow(team_ids)){

  info <- rbind(info, get_info(team_ids$team_id[i]))

  if(i %% 100 == 0) print(i)

}

saveRDS(info, file = "~/Projects/softballR-data/data/ncaa_team_info.RDS")
