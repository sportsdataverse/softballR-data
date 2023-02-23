library(devtools)

devtools::install_github("tmking2002/softballR")

scoreboard <- list()

for(i in 2015:2022){
  
  tic(i)
  scoreboard[[paste(i)]] <- softballR::get_espn_season_scoreboard(i)
  toc()
  
}

saveRDS(object = scoreboard, file = "Past Scoreboards.RDS")
