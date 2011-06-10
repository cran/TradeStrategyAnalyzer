# Example: How to run Simulator, Evaluate RandomBuyHold against MeanRevert Strategy and Visualize

# The data should be in the same directory but in case I test with different dataset.
R_data_dir <- ""

# 1. Make sure required packages are installed
# use onLoad or install.packages but make sure they are installed before continuing
#   e.g., install.packages("DBI")
require(DBI)
require(RSQLite)
require(stats)
require(ggplot2)
require(RJSONIO)
require(googleVis)
require(digest)

# 2. Install our TradeStrategyAnalyzer package
require(TradeStrategyAnalyzer)

# 3. Make sure that sqlite database is set up with the required tables
# .. see create_tables.sql
sqliteDbName <- paste(R_data_dir, "sim.db", sep="")

# 4. Make sure the data needed for the simulator is there
# - Stock Info File, Market Info File
inputStockInfoFile <- paste(R_data_dir, "SP500industry.csv", sep="")
inputMarketInfoFile <- paste(R_data_dir, "SP500market.csv", sep="")
# - Load them in R
StockInfoDataset <- LoadStocksToDB(sqliteDbName, inputStockInfoFile)
StockMarketDataset <- LoadMarketToDB(sqliteDbName, inputMarketInfoFile)

# - Compute and store correlations in DB (in case we need to visualize)
CorrelationsInfo <- MeanRevertCorrelatePairwise(StockMarketDataset)
StoreCorrelationsToDB(sqliteDbName, CorrelationsInfo)

# NOW, WE CAN START EVALUATING DIFFERENT ALGORITHMS
# - for each algorithm, create a different portfolio
# - compare the portfolios' returns after the simulation

# ================== SIMULATE ALGORITHM 1 ===================
# 5.1 ALGORITHM 1: Example to run Random_BuyHold Algorithm
set.seed(100)
portfolioId1 <- 1
algorithmName1 <- "RANDOM_BUY_HOLD"
startAmount <- 100000

PortfoliosInfo <- AddPortfolio(PortfoliosInfo, portfolioId1, algorithmName1, startAmount)

# Compute Trade Actions from Strategy
OrderBook <- DetermineOrders_RandomBuyAndHold(portfolioId1, PortfoliosInfo, StockMarketDataset)

# Simulate Broker
SimResults <- SimulateBroker(portfolioId1, PortfoliosInfo, StockMarketDataset, OrderBook, HoldingsInfo, TransactionsInfo)
HoldingsInfo <- SimResults[[1]]
TransactionsInfo <- SimResults[[2]]

# ================== SIMULATE ALGORITHM 2 ===================
# 5.2 ALGORITHM 2: Example to run MeanRevert Algorithm
portfolioId2 <- 2
algorithmName2 <- "MEAN_REVERT"
startAmount <- 100000
numPairs <- 1

PortfoliosInfo <- AddPortfolio(PortfoliosInfo, portfolioId2, algorithmName2, startAmount)

# Compute Trade Actions from Strategy
OrderBook <- DetermineOrders_MeanRevert(portfolioId2, PortfoliosInfo, StockMarketDataset, numPairs=numPairs)

# Simulate Broker
SimResults <- SimulateBroker(portfolioId2, PortfoliosInfo, StockMarketDataset, OrderBook, HoldingsInfo, TransactionsInfo)
HoldingsInfo <- SimResults[[1]]
TransactionsInfo <- SimResults[[2]]

# ================= END SIMULATION SECTION ==================

# 6. Compare the algorithms by Portfolio Return 
#PrintSimulatorState()
portfolioReturnAlg1 <- GetPortfolioPerformance(portfolioId1, PortfoliosInfo, HoldingsInfo, StockMarketDataset)
portfolioReturnAlg2 <- GetPortfolioPerformance(portfolioId2, PortfoliosInfo, HoldingsInfo, StockMarketDataset)
PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId1,"PERFORMANCE"] <- portfolioReturnAlg1
PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId2,"PERFORMANCE"] <- portfolioReturnAlg2
print(paste("PORTFOLIO ", portfolioId1, algorithmName1, " Overall Return:", portfolioReturnAlg1, "%"))
print(paste("PORTFOLIO ", portfolioId2, algorithmName2, " Overall Return:", portfolioReturnAlg2, "%"))

# 7. This concludes the TradeStrategySimulator part of the package
#   - To visualize the result in the database, run the follow-on TradeStrategyVisualizer package by Tirto Adji
StoreSimulationResults(sqliteDbName, PortfoliosInfo, HoldingsInfo, TransactionsInfo)

# ============================= VISUALIZE RESULTS ====================================
pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

# initialization
# sqlite database
dbFile = sqliteDbName
schemaStocksTableName = "stocks"
schemaMarketTableName = "market"
schemaPortfoliosTableName = "portfolios"
schemaHoldingsTableName = "holdings"
schemaTransactionsTableName = "transactions"
schemaHoldingsValueTableName = "holdingsvalue"
schemaPairCorrelationInfoTableName = "pairCorrelationInfo"
schemaAnnotationsTableName = "annotations"

# bootstrap data
# read TradeStrategySimulator result from DB
stocksData <- readFromDB(dbFile, schemaStocksTableName)
head(stocksData)
portfoliosData <- readFromDB(dbFile, schemaPortfoliosTableName)
head(portfoliosData)
marketData <- readFromDB(dbFile, schemaMarketTableName)
head(marketData)
holdingsData <- readFromDB(dbFile, schemaHoldingsTableName)
head(holdingsData)
transactionsData <- readFromDB(dbFile, schemaTransactionsTableName)
head(transactionsData)
pairCorrelationInfoData <- readFromDB(dbFile, schemaPairCorrelationInfoTableName)
head(pairCorrelationInfoData)
annotationsData <- readFromDB(dbFile, schemaAnnotationsTableName)
head(annotationsData)
histMarketData <- readFromDB(dbFile, schemaMarketTableName,where=" where ticker in (select distinct ticker from transactions)")
head(histMarketData)
sumHoldings <- calcSumHoldings(dbFile, histMarketData,holdingsData)
head(sumHoldings)

#
# visualization
#

# data is from 2009/08/21 - 2010/08/20

## ggplot

# plot holding summary
# with caption
yrng <- range(sumHoldings$x)
xrng <- range(sumHoldings$txdate)
caption <- "Overall Returns:"
for (i in 1:nrow(portfoliosData)) {
   text <- paste(portfoliosData$ALGORITHM[i]," ",round(portfoliosData$PERFORMANCE[i],2),"%",sep="")
   caption <- paste(caption,text,sep="\n")
}
portfolioHoldingSummaryChart(sumHoldings,portfoliosData,xrng,yrng,caption)
pause()

# without caption
portfolioHoldingSummaryChart(sumHoldings,portfoliosData)
pause()


# plot stocks correlation matrix
corrMatrixData <- pairCorrelationInfoData[1:10,]
head(corrMatrixData)
stockCorrMatrixChart(data=corrMatrixData)
pause()

# plot correlated stocks in pairs
sMarketData <- subset(marketData,TICKER %in% corrMatrixData$TICKER1 | TICKER %in% corrMatrixData$TICKER2)
head(sMarketData)
stockCorrPairsCartesianChart(sMarketData,corrMatrixData)
pause()

# cartesian chart of stocks prices that we own
sPrices = subset(marketData,TICKER %in% transactionsData$TICKER)
head(sPrices)
portfolioStocksChart(data=sPrices)
pause()

# boxplot of all the stocks that we own
portfolioBoxPlot(mData=marketData,tData=transactionsData)
pause()

# individual stock price heat map
# stocks from our db
calendarHeatMap(marketData,ticker="A")
pause()
# or any stocks from web services
calendarHeatMap(marketData,ticker="YHOO")
pause()

#
# experimentations with googleVis
#
# set chart title and description
cTitle <- "<h3>Annotated Time Line Portfolio Summary Chart</h3>"
cDesc <- "<br/>Trade Strategy Simulation Result of Mean Reverting vs. Buy Random and Hold Algorithm"
# set chart options
# ZZZ for unknown reasons,min doesn't seem to work as expected
cOpts <- list(displayAnnotations=TRUE,width=800,height=400,min=50000)

gvisATLData <- merge(sumHoldings,portfoliosData,by.x="id",by.y="PORTFOLIOID")
gvisATLData <- merge(x=gvisATLData,y=annotationsData, by=c("id","txdate"), all.x=TRUE)
gvisATLData$title <- NA
head(gvisATLData)
AnnoTimeLine <- gvisAnnotatedTimeLine2(gvisATLData,
   datevar="txdate",
   numvar="x",
   idvar="ALGORITHM",
   titlevar="title",
   annotationvar="annotation",
   options=cOpts,
   charttitle=cTitle,
   chartdesc=cDesc
   )
plot(AnnoTimeLine)
pause()


# motion chart
gvisMCData <- merge(sumHoldings,portfoliosData,by.x="id",by.y="PORTFOLIOID")
gvisMCData$Year <- as.POSIXlt(gvisMCData$txdate)$year + 1900

head(gvisMCData)
state='{"time":"2009-08-21","yZoomedDataMin":98333,"xZoomedDataMax":1282262400000,"sizeOption":"_UNISIZE","orderedByX":false,"dimensions":{"iconDimensions":["dim0"]},"xZoomedIn":false,"yZoomedDataMax":143280,"yZoomedIn":false,"iconType":"BUBBLE","playDuration":15088.88888888889,"showTrails":true,"xAxisOption":"_TIME","iconKeySettings":[{"key":{"dim0":"RANDOM_BUY_HOLD"},"trailStart":"2009-08-21"},{"key":{"dim0":"MEAN_REVERT"},"trailStart":"2009-08-21"}],"yLambda":1,"xZoomedDataMin":1250812800000,"xLambda":1,"nonSelectedAlpha":0.4,"yAxisOption":"3","duration":{"timeUnit":"D","multiplier":1},"orderedByY":false,"uniColorForNonSelected":false,"colorOption":"2"}'

MCtitle <- '<h3>Trade Strategies Comparison Motion Chart</h3>'
MCdesc <- "<br/>Trade Strategy Simulation Result of Mean Reverting vs. Buy Random and Hold Algorithm"
MotionChart <- gvisMotionChart2(gvisMCData, idvar="ALGORITHM", timevar="txdate",date.format = "/%Y-/%m-/%d",options=list(state=state,width=800,height=450),charttitle=MCtitle,chartdesc=MCdesc)
plot(MotionChart)
pause()


# clean ups
rm(stocksData)
rm(portfoliosData)
rm(marketData)
rm(holdingsData)
rm(transactionsData)
rm(pairCorrelationInfoData)
rm(annotationsData)
rm(histMarketData)
rm(sumHoldings)
rm(corrMatrixData)
rm(sMarketData)
rm(sPrices)
rm(xrng,yrng,caption)
rm(cTitle,cDesc,cOpts,gvisATLData)
rm(state,MCtitle,MCdesc,gvisMCData)
