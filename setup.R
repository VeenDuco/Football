# Author comment
# This project is for playing around to predict football scores in different leagues and is based on;
# https://github.com/milkha/EPL/blob/master/epl_rev.Rmd
# http://andrewgelman.com/2017/05/17/using-stan-week-week-updating-estimated-soccer-team-abilites/
#
# File description
# This is the executive file - running all analyses centralized from from here 
#
# Loading packages
library(plyr)
library(dplyr)

# reading in data
source("get_data.r")


# Prepare data for a certain season
source("data_preparation.r")
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
# prior points from: https://en.wikipedia.org/wiki/2014%E2%80%9315_Eredivisie
# all new teams got points equal to best relegated team. 

# store current work:
save.image("store_current_work.RData")
