createD3SpiralConfigJson <- function(configData){
  library(rjson)
  
  getChildren <- function(obj){
    nVectors <- length(names(obj))  
    tableObj <- list()
    for (j in 1:nVectors){
      vectorObj <- list()
      vectorObj <- c(vectorObj, "name" = names(obj)[j])
      vectorObj <- c(vectorObj, "size" = abs(obj[[names(obj)[j]]]$trendBeta))
      tableObj <- c(tableObj, list(vectorObj))
    }
    return(tableObj)
  }
  
  finalObj <- list()
  nTable <- length(names(configData))
  for (i in 1:nTable){
    dbObj <- list()
    dbObj[["name"]] <- names(configData)[i]
    dbObj[["children"]] <- (getChildren(configData[i][[1]]))
    finalObj <- c(finalObj, list(dbObj))
  }  
  mainObj <- list("name" = "flare",
                  "children" = finalObj)
  
  mainObj <- rjson::toJSON(mainObj)
  mainObj <- gsub(",", ",\n", mainObj)
  return(mainObj) 
}
