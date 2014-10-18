complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  result <- data.frame(id = rep(NA, length(id)), nobs = rep(NA, length(id)))
  j <- 1
  for (i in id) {
    fileName <- paste(paste(rep("0", 3-nchar(i)), collapse=""), i, sep="")
    monitorData <- read.csv(paste(sprintf('%s/%s.csv', directory, fileName)))
    ok <- complete.cases(monitorData)
    nobs <- nrow(monitorData[ok,])
    result[j, ] <- c(i, nobs)
    j <- j + 1
  }
  result
}
