# Created by jlrochet on 6-9-2016
# Programming in R, Week 4, Final Assignment

# Task 1
# Creates data.frame with outcomes of care measures data and plots 30 day
# mortality rates for heart attack. 
# I used magrittr package to simplify syntax a bit

library(magrittr)

read.csv("outcome-of-care-measures.csv", colClasses = "character") -> outcome
outcome[,11] %>% as.numeric() -> outcome[,11]
outcome[,11] %>% hist(main = "30 Day Mortality Rate for Heart Attack", xlab = "Days")