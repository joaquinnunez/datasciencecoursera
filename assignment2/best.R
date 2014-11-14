best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) {
        stop("invalid outcome")
    }

    states <- data[, 7]
    states <- unique(states)
    if( state %in% states == FALSE ) {
        stop("invalid state")
    }

    # Renaming the columns names because the names in the file are sooo large
    names(data)[11] <- "heart attack"
    names(data)[17] <- "heart failure"
    names(data)[23] <- "pneumonia"

    ## Return hospital name in that state with lowest 30-day death rate
    data <- data[data$State == state & data[outcome] != "Not Available",]
    values <- data[, outcome]
    minRow <- which.min(values)
    data[minRow, 2]
}
