# Author comment
# This project is for playing around to predict football scores in Dutch league and is based on;
# https://github.com/milkha/EPL/blob/master/epl_rev.Rmd
# http://andrewgelman.com/2017/05/17/using-stan-week-week-updating-estimated-soccer-team-abilites/
#
# File description
# Loading and cleaning data that is to be used in the analyses. 
#
# Loading packages
library(plyr)


# Linear map of points to a score between -1 and 1
map_to_score <- function(x) { 
  x_max <- max(x);   x_min <- min(x);
  return(2*x/(x_max-x_min) - (x_max+x_min)/(x_max-x_min))
}

url_csv <- "http://www.football-data.co.uk/mmz4281/1718/N1.csv"
# Data downloaded from football-data.co.uk
mydat   <- read.csv(url(url_csv))
# teams are assigned IDs 1, 2, ...:

epl <- c()
# teams are assigned IDs 1, 2, ...:
epl$home_team       <- as.numeric(mydat$HomeTeam)  
epl$away_team       <- as.numeric(mydat$AwayTeam)
epl$team_names      <- levels(mydat$HomeTeam)
epl$home_goals      <- mydat$FTHG # FTHG: full time home goals
epl$away_goals      <- mydat$FTAG # FTHG: full time away goals
epl$score_diff      <- epl$home_goals - epl$away_goals
# Points from last season are read and mapped to a score
epl$prev_perf       <- c(81,49,38,37,82,43,43,43,34,76,34,34,45,62,51,34,36,35)
epl$prev_perf       <- map_to_score(epl$prev_perf) 
epl$nteams          <- length(unique(epl$home_team))
epl$ngames          <- length(epl$score_diff)
epl$nweeks          <- floor(2*epl$ngames/epl$nteams)
# The following code computes the week for each team in their games:
epl$home_week <- c();   epl$away_week <- c();
for (g in 1:epl$ngames) {
  epl$home_week[g]  <-  sum(epl$home_team[1:g] == epl$home_team[g]) + 
    sum(epl$away_team[1:g] == epl$home_team[g]) 
  epl$away_week[g]  <-  sum(epl$away_team[1:g] == epl$away_team[g]) +
    sum(epl$home_team[1:g] == epl$away_team[g])
}
epl$bet_home <- mydat$B365H; # Betting odds for home team win
epl$bet_draw <- mydat$B365D; # Betting odds for draw
epl$bet_away <- mydat$B365A; # Betting odds for away team win
saveRDS(epl,'epl_data.rds')
