## This function takes a particular hospital name and finds its rank for three outcomes (heart attack,
## heart failure, and pneumonia). It will return any hospital in any state containing the name
findhospitalrank <- function(hospital){
    ## Read outcome data
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Check to make sure the hospital passed isn't NA/NULL/numeric()
    if(is.na(hospital) | is.numeric(hospital)){
        stop("Inproper datatype")
    }
    # Convert the hospital name given to all uppercase
    hospital <- toupper(hospital)
    check_hospital <- grep(hospital, mydata$Hospital.Name) # returns indices that the hospital exists in
    # if check_hospital is length 0, then the hospital isn't in the database
    
    if(length(check_hospital)==0){ # If the hospital name isn't in the data set, it's invalid
        stop("Hospital Not Available")
    }
    else{
        ## I need to grab the unique states that the hospital name exists in...
        states <- mydata[check_hospital,]$State
        states <- sort(unique(states)) # sort the unique states
        # Initialize bits for the final data frame
        bound_data <- character()
        
        ## Now that I have each state containing this hospital phrase, I need to go through each state
        ## and get the data for each hospital
        for (state in states){
            
            ## Find hospital and grab the Outcome data and Hospital Names
            dataha <- split(mydata[,c(2,11)],mydata$State)[[state]] # Grab heart attacks
            datahf <- split(mydata[,c(2,17)],mydata$State)[[state]] # Grab heart failures
            datapn <- split(mydata[,c(2,23)],mydata$State)[[state]] # Grab pneumonia
            
            ## Convert the second column (mortalities) to numeric characters for sorting
            dataha[,2] <- suppressWarnings(as.numeric(dataha[,2]))
            datahf[,2] <- suppressWarnings(as.numeric(datahf[,2]))
            datapn[,2] <- suppressWarnings(as.numeric(datapn[,2]))
            
            ## Sort the first column in alphabetical order
            # Note that this has to be done because when sorting the second column in ascending order, it
            # goes through from beginning to end.. so when there are ties in the second column I need them
            # to be in alphabetical order and this is the only way to do that.
            sortind_alpha <- sort(dataha$Hospital.Name, index.return=TRUE)$ix
            dataha <- dataha[sortind_alpha,]
            sortind_alphf <- sort(datahf$Hospital.Name, index.return=TRUE)$ix
            datahf <- datahf[sortind_alphf,]
            sortind_alppn <- sort(datapn$Hospital.Name, index.return=TRUE)$ix
            datapn <- datapn[sortind_alppn,]
            
            ## Sort the second column in ascending order
            sortind_morha <- sort(dataha[,2], na.last = TRUE, index.return=TRUE)$ix
            dataha <- dataha[sortind_morha,]
            sortind_morhf <- sort(datahf[,2], na.last = TRUE, index.return=TRUE)$ix
            datahf <- datahf[sortind_morhf,]
            sortind_morpn <- sort(datapn[,2], na.last = TRUE, index.return=TRUE)$ix
            datapn <- datapn[sortind_morpn,]
            
            ## Now our data is ranked in ascending order, so now we just have to find out what rank our
            ## specific hospital(s) has/have and we're good to go!
            rankha <- grep(hospital, dataha$Hospital.Name) # Grab the ranks of each (really their placements)
            rankhf <- grep(hospital, datahf$Hospital.Name)
            rankpn <- grep(hospital, datapn$Hospital.Name)
            
            names <- dataha[rankha,]$Hospital.Name # Grab the hospital names for that state
            
            # First we compile the data in columns, this variable is overwritten
            compiled_data <- cbind(names, state, rankha, rankhf, rankpn)
            # Then we add the compiled data into the row bind with the rest of the data
            bound_data <- rbind(bound_data, compiled_data)
        }
        
        ## Now put the data in a dataframe
        framed_data<-data.frame(bound_data)
        colnames(framed_data) <- c("Hospital Name", "State", "Heart Attack Rank", "Heart Failure Rank", 
                                   "Pneumonia Rank")
        framed_data
    }
}