# This function finds and returns the name of the hospital with the lowest mortality rate in a given state with a
# given outcome (heart attack, heart failure, or pneumonia)
best <- function(state, outcome){
    ## Read outcome data
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Hospital name is column 2 in data
    ## State is column 7 in data
    ## Heart Attack is column 11
    ## Heart Failure is in column 17
    ## Pneumonia is in column 23
    
    ## Check that state and outcome are valid
    if(!(state %in% mydata$State)){ # If the state given isn't in data's States, it's invalid
        stop("Invalid State")
    }
    else if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){ # If the outcome isn't one of these three
        stop("Invalid Outcome")                                               # it's invalid
    }
    # If both the state and the outcome are present in the data, enter this step
    else if(state %in% mydata$State & outcome %in% c("heart attack","heart failure","pneumonia")){
        if (outcome == "heart attack"){ # If the outcome is heart attack
            # This splits the data into a column for hospital name and mortality rate, based on state
            # Then the [[state]] bit grabs the state we care about
            specdata <- split(mydata[,c(2,11)],mydata$State)[[state]] # grab the specified data for the state
            # Now specdata[,1] is Hospital.Name and specdata[,2] is mortality rate
        }
        if (outcome == "heart failure"){ # If the outcome is heart failure
            specdata <- split(mydata[,c(2,17)],mydata$State)[[state]] # grab the specified data for the state
        }
        if (outcome == "pneumonia"){ # If the outcome is pneumonia
            specdata <- split(mydata[,c(2,23)],mydata$State)[[state]] # grab the specified data for the state
        }
    }
    # This is just a catch-all. Shouldn't reach this point but just in case we do
    else{
        stop("Invalid Data")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    # Now we have the specified data in a variable called specdata
    # We want to find the lowest mortality rate, so we just have to find the minimum in specdata
    # The line below finds the rows where mortality rate is equal to the minimum mortality rate
    specdata[,2] <- suppressWarnings(as.numeric(specdata[,2]))
    specdata <- specdata[specdata[,2]==min(specdata[,2],na.rm=TRUE),]
    
    # Now, if there's a tie, we need to make sure it's in alphabetical order
    name <- sort(specdata$Hospital.Name)
    name[1] # Return the first name
}