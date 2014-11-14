# something-apply maybe can be helpful

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) {
        stop("invalid outcome")
    }

    # Renaming the columns names because the names in the file are sooo large
    names(data)[11] <- "heart attack"
    names(data)[17] <- "heart failure"
    names(data)[23] <- "pneumonia"
    # filtering out "Not Available" data
    data <- data[data[outcome] != "Not Available",]

    # Ordering by Hospital Name first, then by outcome
    # outcome column should be numeric
    data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
    data <- data[order(data[, 2], decreasing = FALSE), ]
    data <- data[order(data[outcome], decreasing = FALSE), ]

    ## Helper functiont to process the num argument
    getHospByRank <- function(df, s, n) {
        df <- df[df$State==s, ]
        vals <- df[, outcome]
        if( n == "best" ) {
            rowNum <- which.min(vals)
        } else if( n == "worst" ) {
            rowNum <- which.max(vals)
        } else {
            rowNum <- n
        }
        df[rowNum, 2]
    }

    ## For each state, find the hospital of the given rank
    states <- data[, 7]
    states <- unique(states)
    newdata <- data.frame("hospital"=character(), "state"=character())
    for(st in states) {
        hosp <- getHospByRank(data, st, num)
        newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
    }

    ## Return a data frame with the hospital names and the (abbreviated) state name
    newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
    newdata
}
