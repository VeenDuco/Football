data.preparation <- function(data, prior.points){
  out <- c()
  # Linear map of points to a score between -1 and 1
  map.to.score <- function(x) { 
    x.max <- max(x);   x.min <- min(x);
    return(2*x/(x.max-x.min) - (x.max+x.min)/(x.max-x.min))
  }
  
  out$home_team       <- as.numeric(data$HomeTeam)  
  out$away_team       <- as.numeric(data$AwayTeam)
  out$team_names      <- levels(data$HomeTeam)
  out$home_goals      <- data$FTHG # FTHG: full time home goals
  out$away_goals      <- data$FTAG # FTHG: full time away goals
  out$score_diff      <- out$home_goals - out$away_goals
  # Points from last season are read and mapped to a score
  out$prev_perf       <- prior.points
  print(list(out$team_names,out$prev_perf))
  out$prev_perf       <- map.to.score(out$prev_perf) 
  out$nteams          <- length(unique(out$home_team))
  out$ngames          <- length(out$score_diff)
  out$nweeks          <- floor(2*out$ngames/out$nteams)
  # The following code computes the week for each team in their games:
  out$home_week <- c()   
  out$away_week <- c()
  for (g in 1:out$ngames) {
    out$home_week[g]  <-  sum(out$home_team[1:g] == out$home_team[g]) + 
      sum(out$away_team[1:g] == out$home_team[g]) 
    out$away_week[g]  <-  sum(out$away_team[1:g] == out$away_team[g]) +
      sum(out$home_team[1:g] == out$away_team[g])
  }
  out$bet_home <- data$B365H; # Betting odds for home team win
  out$bet_draw <- data$B365D; # Betting odds for draw
  out$bet_away <- data$B365A; # Betting odds for away team win
  return(out)
}
