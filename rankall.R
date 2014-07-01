rankall <- function(outcome, num = "best") {
    
    ## not sure how I'll use these yet, probably to iterate over somehow
    states <- c("AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
    
    ## outcome can be "heart attack" (col 11), "heart failure" (col 17), or
    ## "pneumonia" (col 23)
    ## set the column for later use
    ## If it is not one of these then throw an error
    if (outcome == "heart attack") {
        column <- 11
    } else if (outcome == "heart failure") {
        column <- 17
    } else if (outcome == "pneumonia") {
        column <- 23
    } else {
        stop ("invalid outcome")
    }
    
    ## Read outcome data
    # no sense in reading the data before we might need it
    outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## take the subset for the state we want
    ## we can't do this since we don't have a state in this case
    ## statedata <- outcomedata[which(outcomedata$State==state),]
    
    ## coerce to numeric since we read is as a string
    outcomedata[,column] <- as.numeric(outcomedata[,column])
    ## sort based on state, outcome, hospital name
    sortedstate <- statedata[order(outcomedata[,7], outcomedata[,column], outcomedata[,2],na.last=NA),]
    ## just the columns that we need
    allhospitals <- sortedstate[, c(2,7,column)]
    
    
    ## we have to turn this into a function
    ## is rank > the actual number of outcomes we have?
    ranked <- function (statedata, pos) {
        
        ## now we can deal with the outcoming ranking
        if (pos == "best") {
            pos <- 1
        } else if (num == "worst") {
            pos <- nrow(statedata)
        } else {
            pos <- as.numeric(pos)
        }      
        
        if (pos > nrow(statedata)) {
            return (c(NA, statedata[1,2]))
        } else {
            return(c(statedata[pos,1], statedata[1,2]))
        }
    }
        
    ## iterate of the data frame by State
    ## not sure yet why I have to transpose this, but it seems to work.
    ranking <- as.data.frame(t(sapply(split (allhospitals, allhospitals[,"State"]), ranked, pos=num)))
    colnames (ranking) <- c("hospital", "state")
        
    
    return (ranking)
  
}
