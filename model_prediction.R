prediction.check <- function(model.list, nweeks, nteams, nsamples = 1500,
                             plot = TRUE){
  # matrix to store predictions in
  score_diff_pred <- array(NA, c(nsamples, (nweeks*(nteams/2))))
  # function to predict future matches
  rt_ls <- function(n, df, mu, a) rt(n,df)*a + mu
  
  # start predicting from week 2. 
  for (j in ((nteams/2)+1):(nweeks*(nteams/2))) {
    week <- ceiling(j/(nteams/2))
    for (i in 1:nsamples) {
      score_diff_pred[i,j] <- 
        rt_ls(1, model.list$nu[i,week-1], 
              model.list$ability.simulations[i,model.list$data.list$home_week[j]-1, 
                                        model.list$data.list$home_team[j]] - 
                model.list$ability.simulations[i,model.list$data.list$away_week[j]-1, 
                                          model.list$data.list$away_team[j]] +
                model.list$b_home[i, week-1],
              model.list$sigma_y[i, week-1])
    }
  }
  
  scd <- model.list$data.list$score_diff[((nteams/2)+1):(nweeks*(nteams/2))]
  scd_sims <- score_diff_pred[, ((nteams/2)+1):(nweeks*(nteams/2))]
  scd_hat <- colMedians(scd_sims)
  scd_se <- sqrt(colVars(scd_sims))
  scd_ub <- scd_hat + 1.95 * scd_se;
  scd_lb <- scd_hat - 1.95 * scd_se;
  scd_ub2 <- scd_hat + 0.67 * scd_se;
  scd_lb2 <- scd_hat - 0.67 * scd_se;
  
  sort_scd <- scd[order(scd)]
  sort_scd_hat <- scd_hat[order(scd)]
  sort_scd_se <- scd_se[order(scd)]
  sort_scd_ub <- scd_ub[order(scd)]
  sort_scd_lb <- scd_lb[order(scd)]
  sort_scd_ub2 <- scd_ub2[order(scd)]
  sort_scd_lb2 <- scd_lb2[order(scd)]
  df <- data.frame(list(scd = sort_scd, scd_hat = sort_scd_hat, scd_se = sort_scd_se, 
                        scd_ub = sort_scd_ub, scd_lb = sort_scd_lb, 
                        scd_ub2 = sort_scd_ub2, scd_lb2 = sort_scd_lb2))
  
  if(plot == TRUE){
  plot1 <- ggplot(df, aes(x = c(((nteams/2)+1):(nweeks*(nteams/2))))) +
    geom_ribbon(aes(ymin = scd_lb,
                    ymax = scd_ub),
                fill="lightyellow") + 
    geom_ribbon(aes(ymin = scd_lb2,
                    ymax = scd_ub2),
                fill="khaki3") + 
    geom_line(aes(y=scd_hat),colour="darkred") + 
    geom_point(aes(y=scd), size = 0.3) +
    scale_x_continuous(name="match") +
    scale_y_continuous(name="score difference", minor_breaks = seq(-6, 6, 1), 
                       sec.axis = dup_axis()) +
    ggtitle("Predicted score differences (red) with 95% intervals (light yellow), \n  50% intervals (dark yellow), and the actual score differences (black)");
  
  print(plot1)
  }
  
  summ <- 0
  sum_vec <- array(0,length(scd))
  scd_h <- round(scd_hat)
  for (i in 1:((nweeks-1)*(nteams/2))) {
    if (scd[i]>0 & scd_h[i]>0)
      summ <- summ + model.list$data.list$bet_home[i+(nteams/2)]
    if (scd[i]<0 & scd_h[i]<0)
      summ <- summ + model.list$data.list$bet_away[i+(nteams/2)]
    if (scd[i]==0 & scd_h[i]==0)
      summ <- summ + model.list$data.list$bet_draw[i+(nteams/2)]
    summ <- summ - 1
    sum_vec[i] <- summ
  }
  print(paste0("Betting each game based on median predicted score with 1$",
               ", your result would be $",
               signif(summ, digits=3)))
  
  if(plot == TRUE){
  plot(c(((nteams/2)+1):(nweeks*(nteams/2))),
       sum_vec,type="l",ylab="cumulative winnings - wager", xlab="match")
  legend("bottomleft",paste0("net results: $", 
                             signif(summ, digits=3)), 
         bty = "n")
  }
  return(signif(summ, digits=3))
}


prediction.check(model.list = england.s1516.E0, nweeks=38,nteams = 20,
                 plot = TRUE)
prediction.check(model.list = netherlands.s1516.E0, nweeks=34,nteams = 18,
                 plot = TRUE)


