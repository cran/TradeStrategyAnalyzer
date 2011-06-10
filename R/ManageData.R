require(DBI)
require(RSQLite)

# CONSTANTS: sqlite database schema settings
#  - table names that match create_tables.sql
schemaStocksTableName <- "stocks"
schemaMarketTableName <- "market"
schemaPortfoliosTableName <- "portfolios"
schemaHoldingsTableName <- "holdings"
schemaTransactionsTableName <- "transactions"
schemaPairCorrelationInfoTableName = "pairCorrelationInfo"

LoadStocksToDB <- function(schemaDbName, inputStockInfoFile) {

	# Read in the csv file
	df <- read.table(inputStockInfoFile, header=TRUE, sep=",", dec=".")
	
	# Write it to database table
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=schemaDbName)
	dbWriteTable(conn, schemaStocksTableName, df, overwrite=TRUE)

	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)
	
	# RETURN data frame
	df
}

LoadMarketToDB <- function(schemaDbName, inputMarketInfoFile) {
	
	# Read in the csv file
	df <- read.table(inputMarketInfoFile, header=TRUE, sep=",", dec=".")
	
	# Data cleaning on input file
	# - want to ensure we only consider stocks that have info for all time units
	tickerlist <- unique(df["TICKER"])
	timelist <- unique(df["ACTIVITY_TS"])
	ntimeunits <- nrow(timelist)
	stockMarket <- data.frame(row.names=timelist)
	numStocks <- 0
	for(i in 1:nrow(tickerlist)) {
		
		# Count number of timeunit entries for this stock
		tickersymbol <- toString(tickerlist[i,"TICKER"])
		numsamples <- length(df[df$TICKER==tickersymbol,"ACTIVITY_TS"])
		
		if(numsamples == ntimeunits) {
			# a stock we want to use
			stockInfo <- data.frame(df[df$TICKER==tickersymbol, ])
			
			if(numStocks == 0) {
				stockMarket <- stockInfo
			} else {
				stockMarket <- rbind(stockMarket, stockInfo)
			}
			numStocks <- numStocks + 1
		}
	}
	
	stockMarket <- stockMarket[order(stockMarket$ACTIVITY_TS),]
	
	# Write it to database table
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=schemaDbName)
	dbWriteTable(conn, schemaMarketTableName, stockMarket, overwrite=TRUE)
	
	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)
	
	# RETURN data frame
	stockMarket
}

StoreCorrelationsToDB <- function(schemaDbName, dfCorrelationsInfo) {
  
	# Write it to database table
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=schemaDbName)
	dbWriteTable(conn, schemaPairCorrelationInfoTableName, dfCorrelationsInfo, overwrite=TRUE)
	
	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)
	
}

StorePortfoliosToDB <- function(schemaDbName, dfPortfoliosInfo) {
	
	# Write it to database table
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=schemaDbName)
	dbWriteTable(conn, schemaPortfoliosTableName, dfPortfoliosInfo, overwrite=TRUE)
	
	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)
	
}

StoreHoldingsToDB <- function(schemaDbName, dfHoldingsInfo) {
	
	# Write it to database table
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=schemaDbName)
	dbWriteTable(conn, schemaHoldingsTableName, dfHoldingsInfo, overwrite=TRUE)
	
	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)
}

StoreTransactionsToDB <- function(schemaDbName, dfTransactionsInfo) {
	
	# Write it to database table
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname=schemaDbName)
	dbWriteTable(conn, schemaTransactionsTableName, dfTransactionsInfo, overwrite=TRUE)
	
	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)
}

StoreSimulationResults <- function(schemaDbName, PortfoliosInfo, HoldingsInfo, TransactionsInfo) {
  StorePortfoliosToDB(schemaDbName, PortfoliosInfo)
  StoreHoldingsToDB(schemaDbName, HoldingsInfo)
  StoreTransactionsToDB(schemaDbName, TransactionsInfo)
}
