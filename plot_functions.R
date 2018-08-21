# get plot functions

individual.team.plot <- function(data.list, ability.matrix, teamname, SE = 1){
# get the ability matrix, mean ability and SE
  dimensions <- dim(ability.matrix)
  a_hat <- matrix(NA, nrow=dimensions[2], ncol=dimensions[3])
  a_se  <- matrix(NA, nrow=dimensions[2], ncol=dimensions[3])
  for (w in 1:dimensions[2]) { 
    a_hat[w,] <- colMeans(ability.matrix[,w,])
    a_se[w,] <- sqrt(colVars(ability.matrix[,w,]))
  }
  a_min <- a_hat - SE * a_se 
  a_max <- a_hat + SE * a_se
  x <- c(1:dimensions[2])
  
  ind <- match(teamname, data.list$team_names)
  plot(a_hat[,ind], type="l", ylim=c(-2.5,2.5),
       lty = 1, lwd = 3, bty='l', xlab=NA, ylab=NA)
  polygon(c(x, rev(x)), c(a_min[,ind], rev(a_max[,ind])), col = 'grey80', border = NA)
  lines(a_hat[,ind], type="l", ylim=c(-2.5,2.5),
        lty = 1, lwd = 3, bty='l')
  title(teamname, line=0)
  par(new = T)
  g1 <- unlist(lapply(ind, function(x) which(data.list$home_team %in% x)))
  g2 <- unlist(lapply(ind, function(x) which(data.list$away_team %in% x)))
  g <- c(g1,g2)
  scd <- data.list$score_diff[g] * c(rep(1, length(g1)), rep(-1, length(g2)))
  aa <- a_hat[,ind]
  scd <- scd[order(g)][1:dimensions[2]]
  plot(scd, col = 2, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.7, ylim=c(-6,6))
  axis(side = 4, col="red",col.axis="red",las=1)
  title(xlab = "week",
        ylab = "team ability",
        outer = TRUE, line = 3, cex.lab=1.5)
  mtext("team ability",side=2,col="black",line=-1.6, outer = TRUE)
  mtext("week",side=1,col="black",line=-2.6, outer = TRUE)
  
}

all.teams.plot <- function(data.list, ability.matrix, SE = 1){
  # to restore parameter settings
  opar <- par(mfrow = c(1,1),
              oma = c(0,0,0,0),
              mar = c(5.1, 4.1, 4.1, 2.1))
  
  # get the ability matrix, mean ability and SE
  dimensions <- dim(ability.matrix)
  a_hat <- matrix(NA, nrow=dimensions[2], ncol=dimensions[3])
  a_se  <- matrix(NA, nrow=dimensions[2], ncol=dimensions[3])
  for (w in 1:dimensions[2]) { 
    a_hat[w,] <- colMeans(ability.matrix[,w,])
    a_se[w,] <- sqrt(colVars(ability.matrix[,w,]))
  }
  a_min <- a_hat - SE * a_se 
  a_max <- a_hat + SE * a_se
  x <- c(1:dimensions[2])
  
  par(mfrow = c(ceiling(dimensions[3]/4),4),
      oma = c(5,4,0,0) + 0.1,
      mar = c(0.8,0.8,4,4) + 0.1) # plot setting needed for plot
  for (i in 1:dimensions[3]) {
    teamname <- data.list$team_names[i]  
    ind <- i
    plot(a_hat[,ind], type="l", ylim=c(-2.5,2.5),
         lty = 1, lwd = 3, bty='l')
    polygon(c(x, rev(x)), c(a_min[,ind], rev(a_max[,ind])), col = 'grey80', border = NA)
    lines(a_hat[,ind], type="l", ylim=c(-2.5,2.5),
          lty = 1, lwd = 3, bty='l')
    title(teamname, line=0)
    
    par(new = T)
    g1 <- unlist(lapply(ind, function(x) which(data.list$home_team %in% x)))
    g2 <- unlist(lapply(ind, function(x) which(data.list$away_team %in% x)))
    g <- c(g1,g2)
    scd <- data.list$score_diff[g] * c(rep(1, length(g1)), rep(-1, length(g2)))
    aa <- a_hat[,ind]
    scd <- scd[order(g)][1:dimensions[2]]
    plot(scd, col = 2, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.7, ylim=c(-6,6))
    axis(side = 4, col="red",col.axis="red",las=1)
  }
  title(xlab = "week",
        ylab = "team ability",
        outer = TRUE, line = 3, cex.lab=1.5)
  mtext("score difference",side=4,col="red",line=-1.5, outer = TRUE) 
  
  par(opar)
}


coef.week.plot <- function(data.list, ability.matrix, week, SE = 1){
  
  i <- week
  # plot ability estimates after week i
    a_hat <- colMeans(ability.matrix[,i,])
    a_se <- sqrt(colVars(ability.matrix[,i,]))
    arm::coefplot (a_hat[order(a_hat)], a_se[order(a_hat)], 
              CI=SE, varnames=data.list$team_names[order(a_hat)],
              main=paste0("Team abilities after week ",i,
                          " (estimate +/- ", SE, " s.e.)\n Teams are sorted according to total achieved points\n"), 
              cex.var=.9, mar=c(1,6,5.1,2), xlim=c(-2.5,2.5),
              cex.pts=2, pch.pts=4)
  
}
