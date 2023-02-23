library(devtools)

devtools::install_github("tmking2002/softballR")

scoreboard <- softballR::get_espn_season_scoreboard(2023)

saveRDS(object = scoreboard, file = "data/Current Season Scoreboard.RDS")
