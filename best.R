# Created by jlrochet on 6-9-2016
# Programming in R, Week 4, Final Assignment

# Task 2
# Creates function called "best" to return character vector with the name
# of the hospital with the lowest 30-day mortality for specified outcome in state

library(magrittr) # I used magrittr package to simplify syntax a bit in places

options(warn = -1) ## Silences the warnings from coercing outcome vectors in lines 20-22

best <- function(state, outcome) {
        ## Read outcome data into outcome variable, coerced as chars
        read.csv("outcome-of-care-measures.csv", colClasses = "character") -> data
        
        ## Coerce outcome vectors for heart attack, heart failure
        ### and pneumonia into numbers
        data[,11] %>% as.numeric() -> data[,11]
        data[,17] %>% as.numeric() -> data[,17]
        data[,23] %>% as.numeric() -> data[,23]
        
        ## Drop unnecessary columns to the right of 23
        data[,1:23] -> data
        
        ## Create vector with valid conditions
        c("heart attack", "heart failure", "pneumonia") -> conditions
        
        ## Check validity of state and outcome arguments
        if((state %in% data$State) == FALSE) stop("invalid state")
        
        if((outcome %in% conditions) == FALSE) stop("invalid outcome")
        
        ## Subset data for relevant state
        
        subset(data, state == data$State) -> data

        ## Order data according to condition, then alphabetically by hospital name
        if (outcome == "heart attack") {
                data[order(data[,11], data[,2]), ] -> data
        }
        
        if (outcome == "heart failure") {
                data[order(data[,17], data[,2]), ] -> data
        }
        
        if (outcome == "pneumonia") {
                data[order(data[,23], data[,2]), ] -> data
        }
        
        ## Return first hospital name
        data[1,2]
        
}