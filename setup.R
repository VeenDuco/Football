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
england.s1516.E0 <- readRDS("FITS/england/s1516/E0/model_after_week38.rds")

netherlands.s1516.E0 <- model.analyses(stan.model = model.m0, nsamples = 1500, 
                                   nweeks = netherlands.s1516$nweeks,
                                   nteams = netherlands.s1516$nteams,
                                   data.list = netherlands.s1516,
                                   country = "netherlands", season = "s1516",
                                   league = "E0")
# save or load the model
netherlands.s1516.E0 <- readRDS("FITS/netherlands/s1516/E0/model_after_week34.rds")

netherlands.s1819.E0 <- model.analyses(stan.model = model.m0, nsamples = 1500, 
                                       nweeks = netherlands.s1819$nweeks,
                                       nteams = netherlands.s1819$nteams,
                                       data.list = netherlands.s1819,
                                       country = "netherlands", season = "s1819",
                                       league = "E0")
# save or load the model
netherlands.s1819.E0 <- readRDS("FITS/netherlands/s1819/E0/model_after_week2.rds")


# Plotting
#https://github.com/milkha/EPL/blob/master/epl_rev.Rmd
#https://github.com/milkha/EPL/blob/master/epl_rev.pdf


source("plot_functions.r")
individual.team.plot(data.list = england.s1516.E0$data.list, 
                     ability.matrix = england.s1516.E0$ability.simulations,
                     teamname = "Arsenal", SE = 1)


individual.team.plot(data.list = netherlands.s1516.E0$data.list, 
                     ability.matrix = netherlands.s1516.E0$ability.simulations,
                     teamname = "Cambuur", SE = 2)

individual.team.plot(data.list = netherlands.s1819.E0$data.list, 
                     ability.matrix = netherlands.s1819.E0$ability.simulations,
                     teamname = "PSV Eindhoven", SE = 2)

all.teams.plot(data.list = netherlands.s1516.E0$data.list, 
               ability.matrix = netherlands.s1516.E0$ability.simulations,
               SE = 2)

coef.week.plot(data.list = netherlands.s1516.E0$data.list, 
               ability.matrix = netherlands.s1516.E0$ability.simulations,
               SE = 2, week = 2)

source("model_checking.r")
model.checking(england.s1516.E0$data.list, 20, 38, "england", "s1516", "E0")
model.checking(netherlands.s1516.E0$data.list, 18, 34, "netherlands", "s1516", "E0")
# test to check with smaller sequences;
model.checking(netherlands.s1819.E0$data.list, 18, 2, "netherlands", "s1819", "E0")
