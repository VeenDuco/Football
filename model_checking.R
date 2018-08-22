model.checking <- function(data.list, nteams, nweeks, country, season, league){
  fit <- readRDS(paste0("FITS/", country, "/", season, "/", league, "/",
                        "fit_", nweeks, ".rds"))
  sims <- extract(fit)
  scd <- data.list$score_diff
  scd_sims <- sims$score_diff_rep
  scd_hat <- colMedians(scd_sims)
  scd_se <- sqrt(colVars(scd_sims))
  alpha <- 0.95
  scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2)
  scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2)
  ci95 <- sum(scd < scd_ub & scd_lb < scd)/((nteams/2) * nweeks)
  alpha <- 0.5
  scd_ub2 <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2)
  scd_lb2 <- colQuantiles(scd_sims, probs = (1-alpha)/2)
  ci50 <- sum(scd < scd_ub2 & scd_lb2 < scd)/((nteams/2) * nweeks)
  
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
  
  plot1 <- ggplot(df, aes(x = c(1:((nteams/2)*nweeks)))) +
    geom_ribbon(aes(ymin = scd_lb,
                    ymax = scd_ub),
                fill="lightyellow") + 
    geom_ribbon(aes(ymin = scd_lb2,
                    ymax = scd_ub2),
                fill="khaki3") + 
    geom_line(aes(y=scd_hat),colour="darkred") + 
    geom_point(aes(y=scd), size = 1) +
    scale_x_continuous(name="match") +
    scale_y_continuous(name="score difference", minor_breaks = seq(-6, 6, 1), 
                       sec.axis = dup_axis()) +
    ggtitle(paste0("Estimated score differences (red) with 95% intervals ",
                   "(light yellow),\n  50% intervals (dark yellow), ",
                   "and the actual score differences (black)"))
  print(plot1)
  print(paste0(signif(100*ci95, digits = 3),"% of observed scores are included",
               " in the 95% CI and ", signif(100*ci50, digits = 3),
               "% of observed scores are included in the 50% CI."))
}
