## This function ranks hospitals in a given state with given outcome (heart attack, heart failure, or pneumonia).
## Given the rank, it will then find whichever place you desire, as given by "num" ("best", "worst", or numeric).
rankhospital <- function(state, outcome, num){
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
    
    # Convert the second column of the data to a numeric
    specdata[,2] <- suppressWarnings(as.numeric(specdata[,2]))
    
    # Now we will do the "best" and "worst" case
    if (num == "best"){
        specdata <- specdata[specdata[,2]==min(specdata[,2],na.rm=TRUE),]
        
        # Now, if there's a tie, we need to make sure it's in alphabetical order
        name <- sort(specdata$Hospital.Name)
        return.name <- name[1] # Return the first name
    }
    else if (num == "worst"){
        specdata <- specdata[specdata[,2]==max(specdata[,2],na.rm=TRUE),]
        
        # Now, if there's a tie, we need to make sure it's in alphabetical order
        name <- sort(specdata$Hospital.Name)
        return.name <- name[1] # Return the first name
    }
    # Now we do a numeric case
    else if (is.numeric(num) & num > 0 & num <= length(specdata$Hospital.Name)){
        # First we have to sort specdata in alphabetical order
        sortind_alp <- sort(specdata$Hospital.Name, index.return=TRUE)$ix
        specdata <- specdata[sortind_alp,]
        # Now that it's in alphabetical order, we can sort it by mortality
        sortind_mor <- sort(specdata[,2], na.last = TRUE, index.return=TRUE)$ix # sort mortality rates from lowest to greatest
        specdata <- specdata[sortind_mor,] # sort it according to the index
        return.name <- specdata$Hospital.Name[num]
    }
    else{
        return(NA)
    }
    return.name
}