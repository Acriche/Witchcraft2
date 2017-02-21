#Restart MySQL:  sudo /usr/local/mysql/support-files/mysql.server restart

setwd("/Users/yoniacriche/Documents/Personal/Witchcraft/Analytic.Engine/Apps/ConfigurationApp/")
source("Wrapper_CreateConfigJson.R")

dbName <- "Shoppers"
tableName <- "transactions"

# run config sync:
configObject <- GenConfigSync(server, dbName, tsTables, dataType, includeTrendData = T,
                                 outputType = "fullData", verbose = T)
