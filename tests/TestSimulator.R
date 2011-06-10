# Example: How to run Simulator and Evaluate RandomBuyHold against MeanRevert Strategy

# The data should be in the same directory but in case I test with different dataset.
R_data_dir <- ""

# 1. Make sure required packages are installed
# use onLoad or install.packages but make sure they are installed before continuing
#   install.packages("DBI")
#   install.packages("RSQLite")
#   install.packages("stats")
require(DBI)
require(RSQLite)
require(stats)
require(ggplot2)
require(RJSONIO)
require(googleVis)
require(digest)

# 2. Install our TradeStrategySimulator package
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
