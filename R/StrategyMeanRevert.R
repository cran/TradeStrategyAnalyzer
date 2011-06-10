require(stats)

MeanRevertCorrelatePairwise <- function(StockMarketDataset, corrThreshold=0.95) {

  	# Convert StockMarketDataset into format for computing correlation
  	tickerlist <- unique(StockMarketDataset["TICKER"])
    timelist <- unique(StockMarketDataset["ACTIVITY_TS"])
  	pricesFrame <- data.frame(row.names=timelist)
  	
  	for(i in 1:nrow(tickerlist)) {
  		
  		tickersymbol <- toString(tickerlist[i,"TICKER"])
  		prices <- data.frame(StockMarketDataset[StockMarketDataset$TICKER==tickersymbol, "PRICE"], row.names=StockMarketDataset[StockMarketDataset$TICKER==tickersymbol, "ACTIVITY_TS"])
  		
  		if(i == 1) {
  			pricesFrame <- prices
  		} else {
  			pricesFrame <- cbind(pricesFrame, prices)
  		}
  		names(pricesFrame)[i] <- tickersymbol
  	}
  	pricesFrame <- pricesFrame[order(row.names(pricesFrame)),]
  	
  	# Compute correlation (NxN matrix)
  	corrFrame <- cor(pricesFrame, method="pearson")
    
    # NOTE: if stdev is zero, there might be some NAs, replace with 0
    corrFrame <- replace(corrFrame, is.na(corrFrame), 0)
    
  	# Replace diagonal with zero (some we don't pick the same stock)
  	numPairs <- 0
  	for(i in 2:nrow(corrFrame)) {
    	rowTicker <- row.names(corrFrame)[i]
    	for(j in 1:(i-1)) {
	      	colTicker <- row.names(corrFrame)[j]
	      	corrValue <- corrFrame[i,j]
	      	absCorrValue <- abs(corrValue)
	      	if(absCorrValue >= corrThreshold) {
	        	# compute Expected diff
	        	diff <- pricesFrame[,rowTicker] - pricesFrame[,colTicker]
	        	expDiff <- mean(diff)
	        	sdDiff <- sd(diff)
	
				pairwise <- data.frame(rowTicker, colTicker, corrValue, absCorrValue, expDiff, sdDiff)
		        names(pairwise)[1] <- "TICKER1"
		        names(pairwise)[2] <- "TICKER2"
		        names(pairwise)[3] <- "CORR"
		        names(pairwise)[4] <- "ABSCORR"
		        names(pairwise)[5] <- "MEANDIFF"
		        names(pairwise)[6] <- "SDEVDIFF"
		        if(numPairs == 0) {
		          pairFrame <- pairwise
		        } else {
		          pairFrame <- rbind(pairFrame, pairwise)
		        }
		        numPairs <- numPairs + 1
	      	}
    	}
  	}

	pairCorrelationInfo <- pairFrame[order(as.matrix(pairFrame$ABSCORR), decreasing=TRUE),]

  	# return (sorted by highest correlation values)
	pairCorrelationInfo
}


# Plot the highest correlation pair
VisualizeCorrelation <- function(pairCorrelationInfo, StockMarketDataset, topPosition) {

  # Sanity check
  if(nrow(pairCorrelationInfo) < topPosition) {
    # we don't have that many pairs
    stop("VisualizeCorrelation: Insufficient pairs (total pairs=", nrow(pairCorrelationInfo), "), requested to display top ranked ", topPosition, " position.", sep="")
  }
  
	ticker1 <- toString(pairCorrelationInfo[topPosition, "TICKER1"])
	ticker2 <- toString(pairCorrelationInfo[topPosition, "TICKER2"])
	ypriceTicker1 <- StockMarketDataset[StockMarketDataset$TICKER==ticker1,"PRICE"]
	ypriceTicker2 <- StockMarketDataset[StockMarketDataset$TICKER==ticker2,"PRICE"]
	xtime <- unique(StockMarketDataset["ACTIVITY_TS"])
	
	par(mfrow=c(2,1))
	plot(xtime, ypriceTicker1, col="blue", xlab="time", ylab="Price")
	title(ticker1)
	plot(xtime, ypriceTicker2, col="green", xlab="time", ylab="Price")
	title(ticker2)
}

DetermineOrders_MeanRevert <- function(portfolioId, PortfoliosInfo, StockMarketDataset, numPairs=1) {
	
	portfolioId <- portfolioId
	startBalance <- as.numeric(PortfoliosInfo[PortfoliosInfo$PORTFOLIOID==portfolioId, "STARTAMOUNT"])
		
	# Compute the Correlations and Pairs
	pairCorrelationInfo <- MeanRevertCorrelatePairwise(StockMarketDataset, corrThreshold=0.95)

	# Only consider positive correlations
	pairCorrelationInfo <- pairCorrelationInfo[pairCorrelationInfo$CORR > 0,]
	
	# how much to allocate for each bet
	pairAllocation <- startBalance / numPairs
	
	# Data frame for maintaining trading state of these pairs
	PairTradeStateInfo <- data.frame(TICKER1=character(0), TICKER2=character(0), CORR=numeric(0), MEANDIFF=numeric(0), SDEVDIFF=numeric(0), STATE=character(0), NUMSHARES1=integer(0), NUMSHARES2=integer(0), stringsAsFactors=FALSE)
	names(PairTradeStateInfo)[1] <- "TICKER1"
	names(PairTradeStateInfo)[2] <- "TICKER2"
	names(PairTradeStateInfo)[3] <- "CORR"
	names(PairTradeStateInfo)[4] <- "MEANDIFF"
	names(PairTradeStateInfo)[5] <- "SDEVDIFF"
	names(PairTradeStateInfo)[6] <- "STATE"
	names(PairTradeStateInfo)[7] <- "NUMSHARES1"
	names(PairTradeStateInfo)[8] <- "NUMSHARES2"

	# Order book
	orderBook <- data.frame(ACTIVITY_TS=character(0), PORTFOLIOID=integer(0), TICKER=character(0), NUMSHARES=integer(0), ORDERTYPE=character(0), ORDERVALUE=numeric(0), stringsAsFactors=FALSE)
	names(orderBook)[1] <- "ACTIVITY_TS"
	names(orderBook)[2] <- "PORTFOLIOID"
	names(orderBook)[3] <- "TICKER"
	names(orderBook)[4] <- "NUMSHARES"
	names(orderBook)[5] <- "ORDERTYPE"
	names(orderBook)[6] <- "ORDERVALUE"

	# Load select pairs and set initial state
	if(numPairs > nrow(pairCorrelationInfo)) {
		numPairs <- nrow(pairCorrelationInfo);
	}
	for(i in 1:numPairs) {
		ticker1 <- pairCorrelationInfo[i, "TICKER1"]
		ticker2 <- pairCorrelationInfo[i, "TICKER2"]
		correlation <- pairCorrelationInfo[i, "CORR"]
		meandiff <- pairCorrelationInfo[i, "MEANDIFF"]
		sdevdiff <- pairCorrelationInfo[i, "SDEVDIFF"]
		# create entry in pair trade state
		pairentry <- data.frame(ticker1, ticker2, correlation, meandiff, sdevdiff, "NEUTRAL", 0, 0, stringsAsFactors=FALSE)
		names(pairentry)[1] <- "TICKER1"
		names(pairentry)[2] <- "TICKER2"
		names(pairentry)[3] <- "CORR"
		names(pairentry)[4] <- "MEANDIFF"
		names(pairentry)[5] <- "SDEVDIFF"
		names(pairentry)[6] <- "STATE"
		names(pairentry)[7] <- "NUMSHARES1"
		names(pairentry)[8] <- "NUMSHARES2"
		PairTradeStateInfo <- rbind(PairTradeStateInfo, pairentry)
	}
	
	# GOING THROUGH EACH TIME UNIT
	#   PairTrade goes through a sequence of allowed states:
    #   - NEUTRAL: can go to OPEN_A or OPEN_B
	#   - OPEN_A: can only go to NEUTRAL
	#   - OPEN_B: can only go to NEUTRAL
	# 
	timeline <- unique(StockMarketDataset$ACTIVITY_TS)
	numtimeunits <- length(timeline)
	
	for(i in 1:numtimeunits) {
		timestamp <- as.character(timeline[i])
		
		for(p in 1:numPairs) {
			ticker1 <- as.character(PairTradeStateInfo[p, "TICKER1"])
			ticker2 <- as.character(PairTradeStateInfo[p, "TICKER2"])
			correlation <- as.numeric(PairTradeStateInfo[p, "CORR"])
			priceTicker1 <- StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp & StockMarketDataset$TICKER==ticker1,"PRICE"]
			priceTicker2 <- StockMarketDataset[StockMarketDataset$ACTIVITY_TS==timestamp & StockMarketDataset$TICKER==ticker2,"PRICE"]	
			meanDiff <- as.numeric(PairTradeStateInfo[p, "MEANDIFF"])
			sdDiff <- as.numeric(PairTradeStateInfo[p, "SDEVDIFF"])
			
			currPriceDiff <- priceTicker1 - priceTicker2
			
			# Detect Recomendations irrespective of Current State
			recommendation <- "NONE"
			if(currPriceDiff > (meanDiff + (2*sdDiff))) {
				# Sell1-Buy2
				recommendation <- "SELL1_BUY2"	
				#print(paste("RECOMMENDS [", timestamp, "] ", ticker1, "-", ticker2, ": 1"))					
				
			} else if (currPriceDiff < (meanDiff - (2*sdDiff))) {
				# Buy1-Sell2
				recommendation <- "BUY1_SELL2"
				#print(paste("RECOMMENDS [", timestamp, "] ", ticker1, "-", ticker2, ": 2"))
			}
			
			# Determine next state based on current state and recommendations
			currentState <- as.character(PairTradeStateInfo[p, "STATE"])
			nextState <- currentState
			action <- "NONE"
			if(currentState == "NEUTRAL") {
				if(recommendation == "SELL1_BUY2") {
					# go to OPEN_A
					nextState <- "OPEN_A"
					action <- "OPEN_A"
				} else if (recommendation == "BUY1_SELL2") {
					# go to OPEN_B
					nextState <- "OPEN_B"
					action <- "OPEN_B"
				}
			} else if (currentState == "OPEN_A") {
				# can only go to CLOSE_A
				if(recommendation == "BUY1_SELL2") {
					# make profit
					nextState <- "NEUTRAL"
					action <- "PROFIT_CLOSE_A"
				}
			} else if (currentState == "OPEN_B") {
				if(recommendation == "SELL1_BUY2") {
					# make profit
					nextState <- "NEUTRAL"
					action <- "PROFIT_CLOSE_B"
				}
			} else {
				# WARNING: invalid state
				print(paste("ERROR: Invalid Current State ", currentState))
			}
			
			# Now we have the actions that need to taken
			if(!action=="NONE") {
				print(paste("[", ticker1, "-", ticker2, "] ACTION: ", action, " , CURR: ", currentState, ", NEXT: ", nextState))
				
				if(action == "OPEN_A") {
					# SELL1_BUY2

					# pick the more expensive one to sell (so won't blow the bank), let that determine the num of shares
					if(priceTicker1 > priceTicker2) {
						numShares1 <- floor(pairAllocation / priceTicker1)
						numShares2 <- numShares1
					} else {
						numShares2 <- floor(pairAllocation / priceTicker2)
						numShares1 <- numShares2
					}
		
					# place 2 orders
					sellOrder <- data.frame(timestamp, portfolioId, ticker1, numShares1, "SELL", priceTicker1)
					names(sellOrder)[1] <- "ACTIVITY_TS"
					names(sellOrder)[2] <- "PORTFOLIOID"
					names(sellOrder)[3] <- "TICKER"
					names(sellOrder)[4] <- "NUMSHARES"
					names(sellOrder)[5] <- "ORDERTYPE"
					names(sellOrder)[6] <- "ORDERVALUE"
					buyOrder <- data.frame(timestamp, portfolioId, ticker2, numShares2, "BUY", priceTicker2)
					names(buyOrder)[1] <- "ACTIVITY_TS"
					names(buyOrder)[2] <- "PORTFOLIOID"
					names(buyOrder)[3] <- "TICKER"
					names(buyOrder)[4] <- "NUMSHARES"
					names(buyOrder)[5] <- "ORDERTYPE"
					names(buyOrder)[6] <- "ORDERVALUE"
					orderBook <- rbind(orderBook, sellOrder, buyOrder)
					
					PairTradeStateInfo[p, "NUMSHARES1"] <- numShares1
					PairTradeStateInfo[p, "NUMSHARES2"] <- numShares2
					
				} else if (action == "OPEN_B") {
					# BUY1_SELL2

					# pick the more expensive one to determine num of shares (so won't blow the bank)
					if(priceTicker1 > priceTicker2) {
						numShares1 <- floor(pairAllocation / priceTicker1)
						numShares2 <- numShares1
					} else {
						numShares2 <- floor(pairAllocation / priceTicker2)
						numShares1 <- numShares2
					}
					
					# place 2 orders
					buyOrder <- data.frame(timestamp, portfolioId, ticker1, numShares1, "BUY", priceTicker1)
					names(buyOrder)[1] <- "ACTIVITY_TS"
					names(buyOrder)[2] <- "PORTFOLIOID"
					names(buyOrder)[3] <- "TICKER"
					names(buyOrder)[4] <- "NUMSHARES"
					names(buyOrder)[5] <- "ORDERTYPE"
					names(buyOrder)[6] <- "ORDERVALUE"
					sellOrder <- data.frame(timestamp, portfolioId, ticker2, numShares2, "SELL", priceTicker2)
					names(sellOrder)[1] <- "ACTIVITY_TS"
					names(sellOrder)[2] <- "PORTFOLIOID"
					names(sellOrder)[3] <- "TICKER"
					names(sellOrder)[4] <- "NUMSHARES"
					names(sellOrder)[5] <- "ORDERTYPE"
					names(sellOrder)[6] <- "ORDERVALUE"
					orderBook <- rbind(orderBook, sellOrder, buyOrder)

					PairTradeStateInfo[p, "NUMSHARES1"] <- numShares1
					PairTradeStateInfo[p, "NUMSHARES2"] <- numShares2
					
				} else if (action == "PROFIT_CLOSE_A") {
					
					# BUY1_SELL2
					numShares1 <- PairTradeStateInfo[p, "NUMSHARES1"]
					numShares2 <- PairTradeStateInfo[p, "NUMSHARES2"]
					
					# place 2 orders
					buyOrder <- data.frame(timestamp, portfolioId, ticker1, numShares1, "BUY", priceTicker1)
					names(buyOrder)[1] <- "ACTIVITY_TS"
					names(buyOrder)[2] <- "PORTFOLIOID"
					names(buyOrder)[3] <- "TICKER"
					names(buyOrder)[4] <- "NUMSHARES"
					names(buyOrder)[5] <- "ORDERTYPE"
					names(buyOrder)[6] <- "ORDERVALUE"
					sellOrder <- data.frame(timestamp, portfolioId, ticker2, numShares2, "SELL", priceTicker2)
					names(sellOrder)[1] <- "ACTIVITY_TS"
					names(sellOrder)[2] <- "PORTFOLIOID"
					names(sellOrder)[3] <- "TICKER"
					names(sellOrder)[4] <- "NUMSHARES"
					names(sellOrder)[5] <- "ORDERTYPE"
					names(sellOrder)[6] <- "ORDERVALUE"
					orderBook <- rbind(orderBook, sellOrder, buyOrder)
					
					PairTradeStateInfo[p, "NUMSHARES1"] <- numShares1
					PairTradeStateInfo[p, "NUMSHARES2"] <- numShares2
					
				} else if (action == "PROFIT_CLOSE_B") {
					# SELL1_BUY2
					numShares1 <- PairTradeStateInfo[p, "NUMSHARES1"]
					numShares2 <- PairTradeStateInfo[p, "NUMSHARES2"]
					
					# place 2 orders
					sellOrder <- data.frame(timestamp, portfolioId, ticker1, numShares1, "SELL", priceTicker1)
					names(sellOrder)[1] <- "ACTIVITY_TS"
					names(sellOrder)[2] <- "PORTFOLIOID"
					names(sellOrder)[3] <- "TICKER"
					names(sellOrder)[4] <- "NUMSHARES"
					names(sellOrder)[5] <- "ORDERTYPE"
					names(sellOrder)[6] <- "ORDERVALUE"
					buyOrder <- data.frame(timestamp, portfolioId, ticker2, numShares2, "BUY", priceTicker2)
					names(buyOrder)[1] <- "ACTIVITY_TS"
					names(buyOrder)[2] <- "PORTFOLIOID"
					names(buyOrder)[3] <- "TICKER"
					names(buyOrder)[4] <- "NUMSHARES"
					names(buyOrder)[5] <- "ORDERTYPE"
					names(buyOrder)[6] <- "ORDERVALUE"
					orderBook <- rbind(orderBook, sellOrder, buyOrder)
					
					PairTradeStateInfo[p, "NUMSHARES1"] <- numShares1
					PairTradeStateInfo[p, "NUMSHARES2"] <- numShares2
				}
				
			}
			
			# Update state
			PairTradeStateInfo[p, "STATE"] <- nextState
			if(nextState == "NEUTRAL") {
				# reset
				PairTradeStateInfo[p, "NUMSHARES1"] <- 0
				PairTradeStateInfo[p, "NUMSHARES2"] <- 0
			}
		}
	}
	
	# RETURN (order book)
	orderBook
}
