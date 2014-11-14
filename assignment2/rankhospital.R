rankhospital <- function(state, outcome, num = "best") {
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
    # filtering out "Not Available" data
    data <- data[data$State == state & data[outcome] != "Not Available",]

    # Ordering by Hospital Name first, then by outcome
    # outcome column should be numeric
    data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
    data <- data[order(data[, 2], decreasing = FALSE), ]
    data <- data[order(data[outcome], decreasing = FALSE), ]

    values <- data[, outcome]
    if( num == "best" ) {
        rowNum <- which.min(values)
    } else if( num == "worst" ) {
        rowNum <- which.max(values)
    } else {
        rowNum <- num
    }

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data[rowNum, 2]
}
