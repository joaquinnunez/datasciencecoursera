corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations

  completeCases <- complete(directory)
  overThresholdCases <- subset(completeCases, completeCases$nobs > threshold)

  # getting the ids of the monitors
  id <- c(overThresholdCases[,1])
  result <- vector("numeric", length = 0)
  for (i in id) {

    fileName <- paste(paste(rep("0", 3-nchar(i)), collapse=""), i, sep="")
    monitorData <- read.csv(paste(sprintf('%s/%s.csv', directory, fileName)))
    monitorDataOk <- na.omit(monitorData)
    correlation <- cor(monitorDataOk$sulfate, monitorDataOk$nitrate)
    result <- append(result, correlation, after = length(result))
  }
  result
}
