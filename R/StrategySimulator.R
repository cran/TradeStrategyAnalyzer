  # Transaction Fees
BuyFee <- 0
SellFee <- 0

AddPortfolio <- function(PortfoliosInfo, portfolioId, algorithm, startAmount) {

   if(nrow(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId,]) > 0) {
     stop(paste("AddPortfolio: Portolio", portfolioId, "already exists")) 
   }
   
   entry <- data.frame(portfolioId, algorithm, startAmount)
   names(entry)[1] <- "PORTFOLIOID"
   names(entry)[2] <- "ALGORITHM"
   names(entry)[3] <- "STARTAMOUNT"
   
   PortfoliosInfo = rbind(PortfoliosInfo, entry)
   
   # return
   PortfoliosInfo
}

SimulateBroker <- function(portfolioId, PortfoliosInfo, StockMarketDataset, OrderBook, HoldingsInfo, TransactionsInfo){
	
	# Temporary data frame for tracking this portfolio changes to holdings
	CurrentHoldingsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), TICKER=character(0), NUMSHARES=integer(0))
	names(CurrentHoldingsInfo)[1] = "PORTFOLIOID"
	names(CurrentHoldingsInfo)[2] = "ACTIVITY_TS"
	names(CurrentHoldingsInfo)[3] = "TICKER"
	names(CurrentHoldingsInfo)[4] = "NUMSHARES"

  # Temporary data from for logging transactions related to this portfolio
  CurrentTransactionsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), DECISION=character(0), TICKER=character(0), NUMSHARES=integer(0))
  names(CurrentTransactionsInfo)[1] = "PORTFOLIOID"
  names(CurrentTransactionsInfo)[2] = "ACTIVITY_TS"
  names(CurrentTransactionsInfo)[3] = "DECISION"
  names(CurrentTransactionsInfo)[4] = "TICKER"
  names(CurrentTransactionsInfo)[5] = "NUMSHARES"

	# Initial Asset is all cash
	previousTimestamp = "1900/01/01 00:00:00"
	startBalance <- as.double(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId, "STARTAMOUNT"])
	entry <- data.frame(portfolioId, previousTimestamp, cashTicker, as.integer(startBalance))
	names(entry)[1] = "PORTFOLIOID"
	names(entry)[2] = "ACTIVITY_TS"
	names(entry)[3] = "TICKER"
	names(entry)[4] = "NUMSHARES"
  CurrentHoldingsInfo <- rbind(CurrentHoldingsInfo, entry)

	timeline <- unique(StockMarketDataset$ACTIVITY_TS)
	numtimeunits <- length(timeline)
	
	# Move sequentially through each time unit
	for(i in 1:numtimeunits) {
		
		# Determine cash available (Cash Ticker)
		balance <- CurrentHoldingsInfo[CurrentHoldingsInfo$TICKER==cashTicker, "NUMSHARES"]
		
		timestamp <- as.character(timeline[i])
		marketInfo <- StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp, ]
		orderInfo <- OrderBook[OrderBook$ACTIVITY_TS==timestamp, ]
		
		# Process orders for the timeunit
		numOrder = nrow(orderInfo)
		if(numOrder > 0) {
			for(o in 1:numOrder) {
				orderType <- as.character(orderInfo[o, "ORDERTYPE"])
				orderTicker <- as.character(orderInfo[o, "TICKER"])
				orderShares <- as.numeric(as.matrix(orderInfo[o, "NUMSHARES"]))   
				if(orderType == "BUY") {
					# This means "buy immediate"
					immediatePrice <- as.numeric(marketInfo[marketInfo$TICKER==orderTicker,"PRICE"])
					transactionTotal <- BuyFee + (immediatePrice*orderShares)		# amount that one has to deduct
					afterBalance <- balance - transactionTotal
					if(afterBalance >= 0) {
						# Have sufficient funds to place trade
						
					    # Look up current
						if(nrow(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,]) > 0) {
							# already has entry for that ticker
							currShares = as.integer(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"])
							nextShares = orderShares + currShares
							# update
							CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"] = nextShares
						} else {
							# add new entry
							entry = data.frame(portfolioId, timestamp, orderTicker, orderShares)
							names(entry)[1] = "PORTFOLIOID"
							names(entry)[2] = "ACTIVITY_TS"
							names(entry)[3] = "TICKER"
							names(entry)[4] = "NUMSHARES"
							CurrentHoldingsInfo = rbind(CurrentHoldingsInfo, entry)
						}
						
						# Stock Order Transaction
						entry <- data.frame(portfolioId, timestamp, "BUY", orderTicker, orderShares)
						names(entry)[1] = "PORTFOLIOID"
						names(entry)[2] = "ACTIVITY_TS"
						names(entry)[3] = "DECISION"
						names(entry)[4] = "TICKER"
						names(entry)[5] = "NUMSHARES"
						CurrentTransactionsInfo <- rbind(CurrentTransactionsInfo, entry)
            
						# Deduct cash
						CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==cashTicker, "NUMSHARES"] = afterBalance
						balance <- afterBalance
						
						# Debug info
						assetValue <- (immediatePrice*orderShares)
						print(paste("BUY [", orderTicker, "] Cash paid (-)", assetValue, " NumShares ", orderShares, ", SharePrice ", immediatePrice))
					} else {
						# DECLINED
						print(paste("ERROR in BUY: [", orderTicker, "] Insufficient cash"))
					}
				} else if (orderType == "SELL") {
					# This means "sell immediate"
					immediatePrice <- as.numeric(marketInfo[marketInfo$TICKER==orderTicker,"PRICE"])
					transactionTotal <- SellFee - (immediatePrice*orderShares)		# amount that one has to deduct (getting money back from sell)
					afterBalance <- balance - transactionTotal
					
					if(afterBalance >= 0) {
						# Have sufficient funds to place trade
						
						# Look up current
						if(nrow(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,]) > 0) {
							# already has entry for that ticker
							currShares = as.integer(CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"])
							nextShares = currShares - orderShares		# SELL ordershares
							# update
							CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==orderTicker,"NUMSHARES"] = nextShares
						} else {
							# add new entry
							entry = data.frame(portfolioId, timestamp, orderTicker, -orderShares)
							names(entry)[1] = "PORTFOLIOID"
							names(entry)[2] = "ACTIVITY_TS"
							names(entry)[3] = "TICKER"
							names(entry)[4] = "NUMSHARES"
							CurrentHoldingsInfo = rbind(CurrentHoldingsInfo, entry)
						}
						
						# Stock Order Transaction
						entry <- data.frame(portfolioId, timestamp, "SELL", orderTicker, orderShares)
						names(entry)[1] = "PORTFOLIOID"
						names(entry)[2] = "ACTIVITY_TS"
						names(entry)[3] = "DECISION"
						names(entry)[4] = "TICKER"
						names(entry)[5] = "NUMSHARES"
						CurrentTransactionsInfo <- rbind(CurrentTransactionsInfo, entry)
					       
						# Deduct cash
						CurrentHoldingsInfo[CurrentHoldingsInfo$PORTFOLIOID==portfolioId & CurrentHoldingsInfo$TICKER==cashTicker, "NUMSHARES"] = afterBalance
						balance <- afterBalance
						
						# Debug info
						assetValue <- (immediatePrice*orderShares)
						print(paste("SELL [", orderTicker, "] Cash gained (+)", assetValue, " NumShares ", orderShares, ", SharePrice ", immediatePrice))
					} else {
						# DECLINED
						print(paste("ERROR in SELL: [", orderTicker, "] Insufficient cash"))
					}
				} else {
					# UNDEFINED ORDER TYPE
					print(paste("Invalid Order Type: [", orderTicker, "] OrderType ", orderType))
				}
			}
		} # numOrder > 0
		
		# Update the timestamp of current holdings
		CurrentHoldingsInfo[, "ACTIVITY_TS"] <- timestamp
		
		# Add it to HoldingsInfo dataset
		HoldingsInfo <- rbind(HoldingsInfo, CurrentHoldingsInfo)
	
		# prepare for next iteration
		previousTimestamp <- timestamp
		
	}
	
  # FIXME: need to check for empty frames in rbind
  TransactionsInfo <- rbind(TransactionsInfo, CurrentTransactionsInfo)
    
	# RETURN (HoldingsInfo and TransactionsInfo data frames)
	list(HoldingsInfo, TransactionsInfo)
}

GetPortfolioWorthAtTime <- function(timestamp, portfolioId, StockMarketDataset, HoldingsInfo) {
	
	holdingsAtTime <- HoldingsInfo[HoldingsInfo$ACTIVITY_TS==timestamp & HoldingsInfo$PORTFOLIOID==portfolioId,]
	numHoldings <- nrow(holdingsAtTime)
	
	total <- 0
	if(numHoldings > 0) {
		
		for(i in 1:numHoldings) {
			holdingTicker = as.character(holdingsAtTime[i,"TICKER"])
			holdingNumShares = as.numeric(holdingsAtTime[i,"NUMSHARES"])
			
			if(holdingTicker == "CASH") {
				total <- total + holdingNumShares	
			} else {
				holdingPrice = StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp & StockMarketDataset$TICKER==holdingTicker,"PRICE"]
				total <- total + (holdingNumShares * holdingPrice)
			}
		}
	}
	
	# RETURN: total amount
	total
	
}

GetPortfolioPerformance <- function(portfolioId, PortfoliosInfo, HoldingsInfo, StockMarketDataset) {
  startPortfolioWorth <- as.double(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId, "STARTAMOUNT"])
  lastTimestamp <- as.character(StockMarketDataset[nrow(StockMarketDataset), "ACTIVITY_TS"])
  endPortfolioWorth <- GetPortfolioWorthAtTime(lastTimestamp, portfolioId, StockMarketDataset, HoldingsInfo)
  overallPortfolioReturn <- 100*(endPortfolioWorth / startPortfolioWorth) - 100

  # RETURN overall return
  overallPortfolioReturn
}
