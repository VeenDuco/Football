model.analyses <- function(stan.model, nsamples = 1500, nweeks, nteams,
                           data.list, country, season, league, save.fits = TRUE){
  # array to store simulated abilities in. 
  ability.simulations <-  array(NA, c(nsamples, nweeks, nteams))
  
  # now we run the models updated per week.
  for(week in 1:nweeks){
    # create an index that says which data needs to be put in the model
    # all data up to and including that week. 2 teams play each other; nteams/2
    index <- c(1:(week*(nteams/2)))
    # now get the data we use up to the week prepared.
    # first get all data
    selected.data <- data.list
    # select relevant data based on index
    selected.data$home_team  <- data.list$home_team[index]
    selected.data$away_team  <- data.list$away_team[index]
    selected.data$home_goals <- data.list$home_goals[index]
    selected.data$away_goals <- data.list$away_goals[index]
    selected.data$score_diff <- data.list$score_diff[index]
    selected.data$home_week  <- data.list$home_week[index]
    selected.data$away_week  <- data.list$away_week[index]
    selected.data$ngames     <- week*(nteams/2)
    selected.data$nweeks     <- max(c(data.list$home_week, data.list$away_week))
    # now get the model samples, adapt_delta higher due to initial small samples
    fit <- sampling(stan.model, chains = 4, iter = (nsamples/2), 
                    data = selected.data, control = list(adapt_delta = .95))
    if(save.fits == TRUE){
    # store full posteriors
    if(!file.exists(paste0("FITS/", country, "/", season, "/", league, "/fit_", 
                           week, ".rds"))){
    saveRDS(fit, paste0("FITS/", country, "/", season, "/", league, "/fit_", 
                        week, ".rds"))
    }
    }
    # now extract the samples for the fit object so we can get the ability's
    samples.fit <- extract(fit)
    # extract the abilities for all teams in the particular week
    for (game in ((week-1) * (nteams/2) + 1):(week * (nteams/2))) {
      # store in ability array all samples, selected week, selected home team
      ability.simulations[, data.list$home_week[game], data.list$home_team[game]] <- 
        samples.fit$a[, data.list$home_week[game], data.list$home_team[game]]
      # store in ability array all samples, selected week, selected away team
      ability.simulations[, data.list$away_week[game], data.list$away_team[game]] <-   
        samples.fit$a[, data.list$away_week[game], data.list$away_team[game]]
    }
  }
  return(ability.simulations)
}


