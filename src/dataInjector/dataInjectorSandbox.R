source("/Users/yoniacriche/Documents/Personal/Witchcraft/Analytic.Engine/Apps/globalWrapper.R")
prodDb <- dbName
dataTables <- tsTables
simulationDbName <- "simulationDb"
startDate <- "2012-03-07"


dataInjector(prodDb = prodDb, dataTables = dataTables, simulationDbName = simulationDbName, 
             sleepTime = 15, 
             timeUnit = "day", startDate = startDate, verbose = T)
  