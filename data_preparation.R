data.preparation <- function(data, prior.points){
  out <- c()
  # Linear map of points to a score between -1 and 1
  map.to.score <- function(x) { 
    x.max <- max(x);   x.min <- min(x);
    return(2*x/(x.max-x.min) - (x.max+x.min)/(x.max-x.min))
  }
  
  out$home.team       <- as.numeric(data$HomeTeam)  
  out$away.team       <- as.numeric(data$AwayTeam)
  out$team.names      <- levels(data$HomeTeam)
  out$home.goals      <- data$FTHG # FTHG: full time home goals
  out$away.goals      <- data$FTAG # FTHG: full time away goals
  out$score.diff      <- out$home.goals - out$away.goals
  # Points from last season are read and mapped to a score
  out$prev.perf       <- prior.points
  print(list(out$team.names,out$prev.perf))
  out$prev.perf       <- map.to.score(out$prev.perf) 
  out$nteams          <- length(unique(out$home.team))
  out$ngames          <- length(out$score.diff)
  out$nweeks          <- floor(2*out$ngames/out$nteams)
  # The following code computes the week for each team in their games:
  out$home.week <- c()   
  out$away.week <- c()
  for (g in 1:out$ngames) {
    out$home.week[g]  <-  sum(out$home.team[1:g] == out$home.team[g]) + 
      sum(out$away.team[1:g] == out$home.team[g]) 
    out$away.week[g]  <-  sum(out$away.team[1:g] == out$away.team[g]) +
      sum(out$home.team[1:g] == out$away.team[g])
  }
  out$bet.home <- data$B365H; # Betting odds for home team win
  out$bet.draw <- data$B365D; # Betting odds for draw
  out$bet.away <- data$B365A; # Betting odds for away team win
  return(out)
}
