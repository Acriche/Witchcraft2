library(RMySQL)
library(jsonlite)

GenConfigSync <- function(server, dbName, tsTables, dataType, 
                          includeTrendData, 
                          outputType = "d3Spiral", verbose = F) {
  
  configJson <- createConfig(server, dbName, tsTables, dataType, 
                             includeTrendData, 
                             outputType = outputType, verbose = verbose)
  # Push to DB:
  if (verbose) cat("\nPushing resutls to DB...")
  insertCmd <- paste0("INSERT INTO WCConfigAnalysis VALUES ('",
                   Sys.time(), "', '",
                   configJson, "'")
  con <- dbConnect(MySQL(), host=server, port=3306, user="salespredictsql" ,password="sales4all", dbname=dbName)    
  df <- dbGetQuery(con, insertCmd)
  dbDisconnect(con)
}

createConfig <- function(server, dbName, tsTables, dataType, 
                             includeTrendData, 
                             outputType = "d3Spiral", verbose){
  # This function will create the json needed for the "Information Spiral".
  # i.e., it'll return the "level of business interest" in each of the 
  # databases/tables/variables, which will be visualized in different 
  # levels of aggregation.
  
  # The "level of business interest" will be measured by the decomposed 
  # trend of the series. Generally - steeper trend will indicate that the
  # series is more "interesting" business wise.
  # todo: change this to something that can handle more complex situations.
  # For numeric variables this is straightforward. For factors, the trend 
  # will be decomposed from the series of "count of unique levels" in the 
  # timeframe. The relevant timeframe is only the lowest level. 
  # (no need for higher level aggregations)
  
  if (verbose) cat("\nStarting createConfig function...")
  
  output <- list()
  for (i in 1:length(tsTables)){
    if (verbose) cat(paste0("\nStarting investigateTable function for ", tsTables[i], "..."))
    res <- investigateTable(server, dbName, tsTables[i], dataType[tsTables[i]][[1]],
                            includeTrendData = includeTrendData, verbose = verbose)
    output[[tsTables[i]]] = res
  }
  
  if (verbose) cat("\nOutputing config to JSON...")
  if (outputType == "d3Spiral"){
    jsonOutput <- createD3SpiralConfigJson(output)
  } else if (outputType == "fullData"){
    jsonOutput <- toJSON(output)
  }
  return(jsonOutput)
}


investigateTable <- function(server, dbName, tableName, tableType, 
                             includeTrendData, verbose){
  # This is the main analysis wrapper function used on each table.
  
  # fetch data:
  con <- dbConnect(MySQL(), host=server, port=3306, user="salespredictsql" ,password="sales4all", dbname=dbName)    
  queryDF <- paste0("select * from ", tableName, " limit 5000")
  df <- dbGetQuery(con, queryDF)
  dbDisconnect(con)
  
  # set df according to design:
  designedDf <- setDataToDesign(df, tableType)
  df <- designedDf$df
  dateColumn <- designedDf$dateColumn
  p <- ncol(df)
  
  trendsList <- list()
 
  # do a time-series analysis on every column in the matrix:
  for (j in 1:p) {
    if (verbose) cat(paste0("\n\tAnalyzing TS for column '", colnames(df)[j], "'"))
    vec <- createTS(df[j], dateColumn, onlyLowerLevelFreq = T) #onlyLowerLevelFreq should be TRUE for the config
    seriesType <- vec$seriesType
    seriesFreq <- names(vec$data)
    vec <- vec$data
    trendData <- getTrend(vec, availableFreqs, includeTrendData)
    trendData$seriesType <- seriesType
    trendData$seriesFreq <- seriesFreq
    trendsList[[colnames(df)[j]]] <- trendData
  }
  
  #todo: analyze correlation between the different ts series   
  
  return(trendsList)
}

