## This function takes a particular outcome (heart attack, heart failure, pneumonia) and ranking, and returns
## a data frame containing all the states and the hospital with that rank in the given state. It utilizes the
## function rankhospital to find the hospital at a given rank
rankall <- function(outcome, num){
    ## Read outcome data
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Hospital name is column 2 in data
    ## State is column 7 in data
    ## Heart Attack is column 11
    ## Heart Failure is in column 17
    ## Pneumonia is in column 23
    
    states <- unique(mydata$State) # Grab all the state names
    states <- sort(states) # Put them in alphabetical order
    hospitals <- character() # Initialize the hosptials vector
    
    ## Check that state and outcome are valid
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ # If the outcome isn't one of these three
        stop("Invalid Outcome")                                               # it's invalid
    }
    else if(outcome %in% c("heart attack","heart failure","pneumonia")){
        for (state in states){
            hospital_name <- rankhospital(state, outcome, num)
            hospitals <- c(hospitals, hospital_name)
        }
    }
    # We shouldn't get here but it's a catch-all
    else{
        stop("Invalid data")
    }
    finaldata <- data.frame(state=states, Hospital=hospitals)
    finaldata
    ## For each state, find the hospital of the given rank
}