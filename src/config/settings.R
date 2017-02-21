# manual advanced config:
########################
server <- "localhost"
dbName <- "Shoppers"
tsTables <- c("history", "transactions")
dataType <- list(history = list(id = "factor",
                                chain = "factor",
                                offer = "factor",
                                market = "factor",
                                repeattrips = "numeric",
                                repeater = "boolean",
                                offerdate = "date",
                                dateFormat = "%Y-%m-%d"),
                 transactions = list(id = "factor",
                                     chain = "factor",
                                     dept = "factor",
                                     category = "factor",
                                     company = "factor",
                                     brand = "factor",
                                     productsize = "factor",
                                     productmeasure = "factor",
                                     purchasequantity = "numeric",
                                     purchaseamount = "numeric",
                                     date = "date",
                                     dateFormat = "%Y-%m-%d"))
