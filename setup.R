# Author comment
# This project is for playing around to predict football scores in different leagues and is based on;
# https://github.com/milkha/EPL/blob/master/epl_rev.Rmd
# http://andrewgelman.com/2017/05/17/using-stan-week-week-updating-estimated-soccer-team-abilites/
#
# File description
# This is the executive file - running all analyses centralized from from here 
#
# Loading packages and setting multiple core estimation to true
library(plyr)
library(dplyr)
library(rstan)
library(arm)
library(matrixStats)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# reading in data
source("get_data.r")

# Prepare data for a certain season
# get function to prepare data
source("data_preparation.r")
#
# prepare specific seasons for use
# teannames order for prior.point: out$team_names <- levels(data$HomeTeam)
england.s1516.pl <- data.preparation(data = england$s1516$E0,
                                     prior.points = c(83,31,42,73,47,
                                                      48,46,52,83,62,
                                                      40,42,54,48,31,
                                                      46,58,42,38,44))

netherlands.s1516 <- data.preparation(data = netherlands$s1516$E0, 
                                      prior.points = c(71,62,41,37,32,
                                                       59,28,46,50,37,
                                                       28,88,28,43,41,
                                                       58,46,53))

#levels(netherlands$s1819$E0$HomeTeam)
netherlands.s1819 <- data.preparation(data = netherlands$s1819$E0, 
                                      prior.points = c(79,71,47,40,30,
                                                       66,30,38,46,42,
                                                       34,83,30,54,49,
                                                       34,37,44))
# prior points from: https://en.wikipedia.org/wiki/2014%E2%80%9315_Eredivisie
# all new teams got points equal to best relegated team. 

# store current work:
# save.image("store_current_work.RData")

# now estimating models over time
# first up - england 15/16 premier league
#
#get model and set samples for posterior
model.m0 <- stan_model("model_m0.stan")
# load function to estimate model
source("model_estimation.r")
# run specific model for specific season
england.s1516.E0 <- model.analyses(stan.model = model.m0, nsamples = 1500, 
                                   nweeks = england.s1516.pl$nweeks,
                                   nteams = england.s1516.pl$nteams,
                                   data.list = england.s1516.pl,
                                   country = "england", season = "s1516",
                                   league = "E0")
# save or load the model
saveRDS(england.s1516.E0, "FITS/england/s1516/E0/abilities.rds")
england.s1516.E0 <- readRDS("FITS/england/s1516/E0/abilities.rds")

netherlands.s1516.E0 <- model.analyses(stan.model = model.m0, nsamples = 1500, 
                                   nweeks = netherlands.s1516$nweeks,
                                   nteams = netherlands.s1516$nteams,
                                   data.list = netherlands.s1516,
                                   country = "netherlands", season = "s1516",
                                   league = "E0")
# save or load the model
saveRDS(netherlands.s1516.E0, "FITS/netherlands/s1516/E0/abilities.rds")
netherlands.s1516.E0 <- readRDS("FITS/netherlands/s1516/E0/abilities.rds")

netherlands.s1819.E0 <- model.analyses(stan.model = model.m0, nsamples = 1500, 
                                       nweeks = netherlands.s1819$nweeks,
                                       nteams = netherlands.s1819$nteams,
                                       data.list = netherlands.s1819,
                                       country = "netherlands", season = "s1819",
                                       league = "E0", save.fits = FALSE)
# save or load the model
saveRDS(netherlands.s1819.E0, "FITS/netherlands/s1819/E0/abilities.rds")
netherlands.s1819.E0 <- readRDS("FITS/netherlands/s1819/E0/abilities.rds")



# Plotting
# plot ability estimates after week 1
a_hat.wk1 <- colMeans(england.s1516.E0[,1,])
a_se.wk1 <- sqrt(colVars(england.s1516.E0[,1,]))
coefplot (a_hat.wk1[order(a_hat.wk1)], a_se[order(a_hat.wk1)], 
          CI=1, varnames=england.s1516.pl$team_names[order(a_hat.wk1)],
          main="Team abilities after week 1 (estimate +/- 1 s.e.)\n Teams are sorted according to previous performance\n", 
          cex.var=.9, mar=c(1,6,5.1,2), xlim=c(-2,2))
for(i in 2:38){
# plot ability estimates after weeks 2-38
a_hat <- colMeans(england.s1516.E0[,i,])
a_se <- sqrt(colVars(england.s1516.E0[,i,]))
coefplot (a_hat[order(a_hat.wk1)], a_se[order(a_hat.wk1)], 
               CI=1, varnames=england.s1516.pl$team_names[order(a_hat.wk1)],
               main=paste0("Team abilities after week ",i,
                           " (estimate +/- 1 s.e.)\n Teams are sorted according to total achieved points\n"), 
               cex.var=.9, mar=c(1,6,5.1,2), xlim=c(-2,2))
}
#https://github.com/milkha/EPL/blob/master/epl_rev.Rmd
#https://github.com/milkha/EPL/blob/master/epl_rev.pdf


source("plot_functions.r")
individual.team.plot(data.list = england.s1516.pl, 
                     ability.matrix = england.s1516.E0,
                     teamname = "Arsenal", SE = 1)


individual.team.plot(data.list = netherlands.s1516, 
                     ability.matrix = netherlands.s1516.E0,
                     teamname = "Cambuur", SE = 2)

individual.team.plot(data.list = netherlands.s1819, 
                     ability.matrix = netherlands.s1819.E0,
                     teamname = "PSV Eindhoven", SE = 2)

all.teams.plot(data.list = netherlands.s1516, 
               ability.matrix = netherlands.s1516.E0,
               SE = 2)

coef.week.plot(data.list = netherlands.s1516, 
               ability.matrix = netherlands.s1516.E0,
               SE = 2, week = 2)
