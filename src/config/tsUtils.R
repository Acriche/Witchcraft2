#This file contains a collection of VECTOR-level functions.

createTS <- function(vec, dateVec, onlyLowerLevelFreq){
  # This function will create several time series for every variable.
  # For numeric variables it will create a ts series of SUM, for each of the time
  # frames aggregation levels available (day, week, month, quarter, year).
  # For factors, it will create a series of count for each of the levels in the 
  # factor (i.e. create an indicator matrix). For each of the indicators, it will
  # will create a numeric ts, for each of the aggregation levels, as described above.
  # In case the factor is too sparse (e.g. ID variable) then it will create a single
  # time-series of COUNT.
  
  # Regarding new levels in factors (e.g., new campaign id, new product id, etc.) - these ts
  # will have a dedicated algorithm which will compare them from the get-go to the other time-
  # series within this factor.  
  availableAggregatedTimes <<- c(1, 4, 12, 52, 365.25)
  
  #'frequency' is the number of observations per season.
  availableFreqs <<- list("365.25" = c(7, 30, 365.25), #daily series
                          "52" = c(52), #weekly series
                           "12" = c(12), #monthly series
                           "4" = c(4), #quartly series
                           "1" = c(1) #yearly series
                          )
  vec <- vec[order(dateVec, decreasing = F), ]
  dateVec <- dateVec[order(dateVec, decreasing = F)]
  
  timeFreq <- findTimeFreq(dateVec)
  
  if (is.numeric(vec)){
    res <- createSumTS(vec, dateVec, timeFreq, onlyLowerLevelFreq)
    seriesType = "Sum"
  } else if (is.factor(vec)){
    res <- creatCountUniqueTS(vec, dateVec, timeFreq, onlyLowerLevelFreq)
    seriesType = "Count Unique"
  } else if (is.logical(vec)){
    res <- createSumTS(vec, dateVec, timeFreq, onlyLowerLevelFreq)
    seriesType = "Sum"
  }
  
  return(list("data" = res,
         "seriesType" = seriesType))
}

findTimeFreq <- function(dateVec){
  # This function will return the maximal seasonality period available in the series.
  # e.g., for daily data, the maximal seasonality freq is 365.25 days.
  # todo: make sure that the timeFreq chosen is eqaul for the entire series,
  #       i.e., that the differences in obs are equal
  
  # An improvment to this function will be to look only at the UNIQUE VALUES
  # of every time series. This will help us handle series which, for example, 
  # seem to have an update every day, but really have a new value every month.
  # (this comment applies only for factor series)
  
  # At the moment this does not support time freq's lower than 1 day (e.g. hours etc.)
  
  dateVec <- dateVec[order(dateVec, decreasing = F)]
  dateVec <- dateVec[!duplicated(dateVec)]
  diff <- dateVec[-1] - dateVec[-length(dateVec)]
  diff <- as.numeric(diff)
  timeFreq <- min(diff)
  denominator <- 365.25 / timeFreq
  return(denominator)
}

createSumTS <- function(vec, dateVec, timeFreq, onlyLowerLevelFreq){
  # For numeric variables this function will create a time-series
  # of SUM, for EACH of the time frames aggregation levels available 
  # (day, week, month, quarter, year), which are higher than timeFreq.
  # In case onlyLowerLevelFreq == T then only the lowest frequancy will
  # be created.
  # Note: a cummulative series is not interesting in the analysis level
  #       (because it's predictions are based on the non-cummulative 
  #       level). However, it may be interesting for UI.
  
  # Find the aggregated time freq higher than timeFreq:
  higherTimeFreq <- findHigherTimeFreq(timeFreq, availableAggregatedTimes)
  allTimeFreqs <- c(timeFreq, higherTimeFreq)
  allTimeFreqs <- allTimeFreqs[order(allTimeFreqs, decreasing = T)]
  
  if (onlyLowerLevelFreq) 
    allTimeFreqs <- max(allTimeFreqs)
  
  # Create a SUM time series for each aggregated time frame:
  seriesList <- createMultipleSumSeries(vec, dateVec, timeFreq, allTimeFreqs)
  
  return(seriesList)
}

creatCountUniqueTS <- function(vec, dateVec, timeFreq, onlyLowerLevelFreq){
  # This function will create a time series of "COUNT of unique levels".
  # e.g. number of unique user id's per day.
  # This function does not go deeper into creating the lower-level series
  # of creating a time series per level. (e.g. counting the number of appearances
  # of a certian id)
  
  higherTimeFreq <- findHigherTimeFreq(timeFreq, availableAggregatedTimes)
  allTimeFreqs <- c(timeFreq, higherTimeFreq)
  allTimeFreqs <- allTimeFreqs[order(allTimeFreqs, decreasing = T)]
  
  if (onlyLowerLevelFreq) 
    allTimeFreqs <- max(allTimeFreqs)
  
  # Create a COUNT time series for each aggregated time frame:
  seriesList <- createMultipleCountSeries(vec, dateVec, timeFreq, allTimeFreqs)
  
  return(seriesList)
}


findHigherTimeFreq <- function(timeFreq, availableAggregatedTimes){
  # This function will find the lower level frequancies are available.
  higherTimeFreq <- availableAggregatedTimes[availableAggregatedTimes < timeFreq]
  return(higherTimeFreq)
}

createMultipleSumSeries <- function(vec, dateVec, timeFreq, allTimeFreqs){
  # This function will create multiple sum series for each aggregation level
  # in the allTimeFreqs object. It is a wrapper for createAggregatedSumSeries.
  
  res <- list()
  for (t in 1:length(allTimeFreqs)){
    timeAsString <- convertNumericTimeToString(allTimeFreqs[t])
    series_i <- createAggregatedSumSeries(vec, dateVec, timeAsString)
    res_i <- list(results = series_i)
    names(res_i) <- allTimeFreqs[t]
    res <- c(res, res_i)
  }
  return(res)
}

createMultipleCountSeries <- function(vec, dateVec, timeFreq, allTimeFreqs){
  # This function will create multiple sum series for each aggregation level
  # in the allTimeFreqs object. It is a wrapper for createAggregatedSumSeries.
  
  res <- list()
  for (t in 1:length(allTimeFreqs)){
    timeAsString <- convertNumericTimeToString(allTimeFreqs[t])
    series_i <- createAggregatedCountSeries(vec, dateVec, timeAsString)
    res_i <- list(results = series_i)
    names(res_i) <- allTimeFreqs[t]
    res <- c(res, res_i)
  }
  return(res)
}

convertNumericTimeToString <- function(numericTime){
  # This function will convert numeric time into string.
  # For example: 30/365.25 will convert to "%m"
  # If it's not a known freq (e.g, every 2 days) then the
  # function will return the UPPER closest frequancy (in 
  # this case, a week)
  
  stringTimeOptions <- c("%Y", "Quarter", "%m", "%U", "Day")
  numericTimeOptions <-availableAggregatedTimes
  if (numericTime %in% numericTimeOptions){
    return(stringTimeOptions[numericTimeOptions %in% numericTime])
  } else {
    # Return the upper closest frequancy:
    diff <- numericTimeOptions - numericTime
    minPosDiff <- min(diff[diff >= 0])
    closestFreq <- numericTimeOptions[diff == minPosDiff]
    return(closestFreq)
  }
}

createAggregatedSumSeries <- function(vec, dateVec, timeAsString){
  # This function will create an aggregated SUM series according to timeAsString.
  # todo: We need to create zero values for missing time windows. e.g., for no 
  #       reports on Saturday.
  
  library(plyr)
  library(zoo)
  tempDf <- data.frame(vec = vec, dateVec = dateVec)
  if (sum(timeAsString %in% c("%U", "%m", "%Y")) > 0){
    tempDf$aggregatedTime <- format(tempDf$dateVec, timeAsString)
  } else if (sum(timeAsString %in% c("Day")) > 0){
    tempDf$aggregatedTime <- strftime(tempDf$dateVec, format = "%j")
  } else if (timeAsString == "Quarter") {
    tempDf$aggregatedTime <- as.yearqtr(tempDf$dateVec)
  }
  if (sum(timeAsString %in% c("%Y", "Quarter")) == 0){
    tempDf$aggregatedTime <- paste0(format(tempDf$dateVec, "%Y"), 
                                    " ",
                                    tempDf$aggregatedTime)
  }
  res <- ddply(tempDf, .(aggregatedTime), summarise, 
               aggregatedVec = sum(vec),
               minDate = min(dateVec))
  return(res)
}

createAggregatedCountSeries <- function(vec, dateVec, timeAsString){
  # This function will create an aggregated COUNT series according to timeAsString.
  
  library(plyr)
  library(zoo)
  tempDf <- data.frame(vec = vec, dateVec = dateVec)
  if (sum(timeAsString %in% c("%U", "%m", "%Y")) > 0){
    tempDf$aggregatedTime <- format(tempDf$dateVec, timeAsString)
  } else if (sum(timeAsString %in% c("Day")) > 0){
    tempDf$aggregatedTime <- strftime(tempDf$dateVec, format = "%j")
  } else if (timeAsString == "Quarter") {
    tempDf$aggregatedTime <- as.yearqtr(tempDf$dateVec)
  }
  if (sum(timeAsString %in% c("%Y", "Quarter")) == 0){
    tempDf$aggregatedTime <- paste0(format(tempDf$dateVec, "%Y"), 
                                    " ",
                                    tempDf$aggregatedTime)
  }
  res <- ddply(tempDf, .(aggregatedTime), function(x){
              aggregatedVec <- length(unique(x$vec))
              minDate <- min(x$dateVec)
              r <- data.frame("aggregatedVec" = aggregatedVec,
                              "minDate" = minDate)
              return(r)
            })
  return(res)
}

getTrend <- function(series, availableFreqs, includeTrendData, nPoints = 10){
  # This function will decompose the time series and return 
  # the beta of the trend.
  
  library(forecast)
  
  seriesTimeFreq <- as.numeric(names(series))
  v <- series[[1]]
  
  # Remove the first and last obs for suspected incompleteness.
  # todo: This should be taken care of later on.
  v <- v[-1, ]
  v <- v[-nrow(v), ]

  # available time freqs for possible seasonality: (for now - take only one of them)
  seasonalPeriods <- min(availableFreqs[names(availableFreqs) %in% seriesTimeFreq][[1]])
  
  #create a ts with muliple possible frequancies:
  ts1 <- msts(v$aggregatedVec, seasonal.periods = seasonalPeriods)
  fit <- stl(ts1, s.window="periodic")
  decomposeDf <- as.data.frame(fit$time.series)
  
  if (includeTrendData) {
    trendData <- data.frame(date = v$minDate, 
                            series = v$aggregatedVec, 
                            trend = decomposeDf$trend)
    #divide into nPoints, by computing the average in each window:
#     trendData$date <- as.Date(trendData$date)
#     trendData2 <- zoo(trendData)
#     trendData2 <- rollmean(trendData[-1], 37)
#       rollmedian(trendData[-1], 10)
#       rollapply(trendData$series, width = 3, by = 6, 
#                             FUN = mean, align = "left")
#     trendData$series
  } else {
    trendData <- NULL
  }
  #plot(decomposeDf$trend)
  trendBeta <- (decomposeDf$trend[nrow(decomposeDf)] - decomposeDf$trend[1]) / nrow(decomposeDf)
  #todo: the beta should be changed to normalized integral

  r <- list("trendBeta" = trendBeta,
            "trendData" = trendData)
  
  return(r)
}


setDataToDesign <- function(df, tableType){
  
  # set date column:
  dateColumn <- df[names(tableType)[tableType == "date"]]
  dateColumn <- as.Date(dateColumn[,1], format = tableType[names(tableType) == "dateFormat"][[1]])
  df <- df[!(colnames(df) %in% names(tableType)[tableType == "date"])]
  
  # set nurmeric columns as numeric:
  df[names(tableType)[tableType == "numeric"]] <- data.frame(
    lapply(df[names(tableType)[tableType == "numeric"]], function(x) as.numeric(as.character(x))),
    check.names = F)
  
  # set factors columns as factors:
  df[names(tableType)[tableType == "factor"]] <- data.frame(
    lapply(df[names(tableType)[tableType == "factor"]], as.factor),
    check.names = F)
  
  # set boolean columns as factors:
  df[names(tableType)[tableType == "boolean"]] <- data.frame(
    lapply(df[names(tableType)[tableType == "boolean"]], as.factor),
    check.names = F)
  
  return(list("df" = df,
              "dateColumn" = dateColumn))
}

