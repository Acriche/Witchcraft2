deleteAllTablesInDb <- function(con, dbName){
  dbSendQuery(con, paste0("DROP DATABASE ", dbName))
  dbSendQuery(con, paste0("CREATE DATABASE ", dbName))
}

createTableFromDesign <- function(con, prodDb, simDb, table){
  dbSendQuery(con,
              paste0("CREATE TABLE ", simDb , ".", table,
                     " LIKE ", prodDb, ".", table ,";")
              )
}

selectTimeQuery <- function(opp = "*", prodDb, table, timeCol,
                            startTime, endTime){
  if (is.null(startTime)){
    s1 <- paste0(" select ", opp, " from ", prodDb, ".", table, 
                 " where ", timeCol, "<='", endTime, "'")
  } else {
    s1 <- paste0(" select ", opp, " from ", prodDb, ".", table, 
                 " where ", timeCol, ">'", startTime, "'",
                 " and ", timeCol, "<='", endTime, "'")
    
  }
  return(s1)
}

injectTimeSpan <- function(con, prodDb, simDb, table, timeCol, 
                           startTime = NULL, endTime, verbose){
  
  s1 <- selectTimeQuery(opp = "*", prodDb, table, timeCol,
                        startTime, endTime)
  s2 <- paste0("insert into ", simDb, ".", table, s1)
  dbSendQuery(con, s2)
  
  if (verbose){
    c1 <- selectTimeQuery(opp = "count(*)", prodDb, table, timeCol,
                          startTime, endTime)
    c1Res <- dbGetQuery(con, c1)
    cat(paste0("\n\tTable '", table, "': finished injecting ", c1Res, "records"))
  }
}

findMaxDate <- function(con, prodDb, tables, tableDesign){
  maxDate <- as.Date(-20000000)
  for (i in 1:length(tables)){
    timeCol <- names(tableDesign[tables[i]][[1]])[as.logical(tableDesign[tables[i]][[1]]=="date")]
    q <- paste0("select max(", timeCol, ") from ", tables[i])  
    maxDate_i <- as.Date(as.character(dbGetQuery(con, q)))
    if (maxDate_i > maxDate) maxDate <- maxDate_i
  }
  return(maxDate)
}