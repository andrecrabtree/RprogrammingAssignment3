rankhospital <- function(state, outcome, num = "best") {
  
  ## Check that state and outcome are valid
  ## If the state is not a valid state then throw an exception
  states <- c("AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  if (!is.element(state, states)) stop ("invalid state")
  
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
  statedata <- outcomedata[which(outcomedata$State==state),]
  ## coerce to numeric since we read is as a string
  statedata[,column] <- as.numeric(statedata[,column])
  ## sort based on state column
  sortedstate <- statedata[order(statedata[,column], statedata[,2],na.last=NA),]
  
  ## now we can deal with the outcoming ranking
  if (num == "best") {
      rank <- 1
  } else if (num == "worst") {
      rank <- nrow(sortedstate)
  } else {
      rank <- as.numeric(num)
  }
  
  ## is rank > the actual number of outcomes we have?
  if (rank > nrow(sortedstate)) {
      return (NA)
  } else {
      return(sortedstate[rank,2])
      
  }
  
  
  
}
