# Created by jlrochet on 6-14-2016
# Programming in R, Week 4, Final Assignment

# Task 4
# Creates function called "rankall" to return data frame with a list of hospital
# names and state abbreviations corresponding to the specified outcome and
# per state ranking

library(magrittr) # I used magrittr package to simplify syntax a bit in places

rankhospital <- function(outcome, num = "best") {
        ## Read outcome data into outcome variable, coerced as chars
        read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE) -> data
        
        ## Drop unnecessary columns within data
        data[,c(2,7,11,17,23)] -> data
        
        ## Rename columns for simplicity
        
        c("hospital", "state", "heart attack", "heart failure", "pneumonia") -> names(data)
        
        ## Create vector with valid conditions
        c("heart attack", "heart failure", "pneumonia") -> conditions
        
        ## Check validity of outcome argument
        if((outcome %in% conditions) == FALSE) stop("invalid outcome")
        
        ## Order data by state, by condition, and by hospital. Drop NA's for relevant condition.
        ## Assign column value for relevant condition.
        
        outcomeCol <- NULL
        
        if (outcome == "heart attack") {
                data[order(data[,"state"], data[,"heart attack"], data[,"hospital"]), ] -> data
                data[complete.cases(data[,"heart attack"]), ] -> data
                3 -> outcomeCol
        }
        
        if (outcome == "heart failure") {
                data[order(data[,"state"], data[,"heart failure"], data[,"hospital"]), ] -> data
                data[complete.cases(data[,"heart failure"]), ] -> data
                4 -> outcomeCol
        }
        
        if (outcome == "pneumonia") {
                data[order(data[,"state"], data[,"pneumonia"], data[,"hospital"]), ] -> data
                data[complete.cases(data[,"pneumonia"]), ] -> data
                5 -> outcomeCol
        }
        
        ## Create list with data by state
        statedata <- split(data, data$state)  
        
        ## Define num value for "best"
        
        if (num == "best") {
                1 -> num
        }
        
        ## Create vector with hospital name from each state according to ranking,
        ## with a special loop implementation for "worst" condition and
        ## using sapply for other conditions
        
        if (num == "worst") {
                lastranking <- c(1:54)
                
                for (i in 1:54) {
                        lastranking[i] <- length(statedata[[i]][[1]])
                }
                
                for (i in 1:54) {
                        hospitalvector[i] <- statedata[[i]][["hospital"]][[lastranking[[i]]]]
                }
        } else {
                hospitalvector <- sapply(statedata, function(tab) tab[num,1])
        }
        
        ## Create vector of states in same order
        
        statevector <- data[,2]
        statevector <- unique(statevector)
        statevector <- statevector[order(statevector)]
        
        ## Create data frame with the desired ranking from each state
        
        ranking <- data.frame(hospital = hospitalvector, state = statevector)
        
        ## Return data frame
        ranking
}