dataInjector <- function(prodDb, dataTables, simulationDbName, sleepTime, 
                         timeUnit = "day", startDate, verbose = F){
  # This function is emulate the activity of a live data wharehouse by
  # injecting data into a simulation db every 'sleepTime' period.
  # For now this is only reffering to timeUnit = "Day"
  
  #connect to db:
  if (verbose) cat("\nConnecting to db...")
  con <- dbConnect(MySQL(), host=server, port=3306, user="" ,password="", dbname=dbName)    
  
  #clean simulationDb:
  if (verbose) cat(paste0("\nRecreating db '", simulationDbName, "'."))
  deleteAllTablesInDb(con, simulationDbName)
  
  #recreate tables in simulationDbName:
  if (verbose) cat("\nRecreating tables.")
  for (i in 1:length(dataTables)){
    createTableFromDesign(con, prodDb = prodDb, 
                          simDb = simulationDbName, tsTables[i])
  }
  
  #inject initial dataset:
  if (verbose) cat("\nInjecting initial dataset:")
  for (i in 1:length(dataTables)){
    timeCol <- names(dataType[dataTables[i]][[1]])[as.logical(dataType[dataTables[i]][[1]]=="date")]
    injectTimeSpan(con, prodDb, simulationDbName, dataTables[i], 
                   timeCol = timeCol, startTime = NULL, endTime = startDate, verbose = verbose)
  }
  
  #inject loop:
  if (verbose) cat("\nStarting inject loop...")
  maxDate <- findMaxDate(con = con, prodDb = prodDb, tables = dataTables, 
                         tableDesign = dataType)
  currentDate <- as.Date(startDate)
  while(currentDate < maxDate){
    for (i in 1:length(dataTables)){
      timeCol <- names(dataType[dataTables[i]][[1]])[as.logical(dataType[dataTables[i]][[1]]=="date")]
      injectTimeSpan(con, prodDb, simulationDbName, dataTables[i], 
                     timeCol = timeCol, startTime = currentDate, endTime = (currentDate + 1), 
                     verbose = verbose)  
    }
    if (verbose) cat(paste0("\nFinished injecting data until ", currentDate, "."))
    currentDate <- currentDate + 1
    Sys.sleep(sleepTime)
  }
  
  dbDisconnect(con)
}
