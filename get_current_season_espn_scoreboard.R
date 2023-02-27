install.packages("devtools", repos = "http://cran.us.r-project.org")
library(devtools)

source_url("https://raw.githubusercontent.com/tmking2002/softballR/main/R/get_espn_season_scoreboard.R")

scoreboard <- get_espn_season_scoreboard(2023)

saveRDS(object = scoreboard, file = "data/espn_scoreboard_2023.RDS")
