# ---- STATE MAINTAINED BY STRATEGY SIMULATOR ----
# Useful for developers when debugging their algorithm
# FIXME: In the future, I should put this is a class

# Data Frame with General Info about Portfolios
#   PortfolioId, Algorithm, StartAmount
PortfoliosInfo <- data.frame(PortfolioID=integer(0), Algorithm=character(0), StartAmount=double(0), PERFORMANCE=double(0))
names(PortfoliosInfo)[1] = "PORTFOLIOID"
names(PortfoliosInfo)[2] = "ALGORITHM"
names(PortfoliosInfo)[3] = "STARTAMOUNT"
names(PortfoliosInfo)[4] = "PERFORMANCE"

# Data Frame with Holdings info
HoldingsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), TICKER=character(0), NUMSHARES=integer(0))
names(HoldingsInfo)[1] = "PORTFOLIOID"
names(HoldingsInfo)[2] = "ACTIVITY_TS"
names(HoldingsInfo)[3] = "TICKER"
names(HoldingsInfo)[4] = "NUMSHARES"

# Data Frame with Transactions info
TransactionsInfo <- data.frame(PORTFOLIOID=integer(0), ACTIVITY_TS=character(0), DECISION=character(0), TICKER=character(0), NUMSHARES=integer(0))
names(TransactionsInfo)[1] = "PORTFOLIOID"
names(TransactionsInfo)[2] = "ACTIVITY_TS"
names(TransactionsInfo)[3] = "DECISION"
names(TransactionsInfo)[4] = "TICKER"
names(TransactionsInfo)[5] = "NUMSHARES"

# CONSTANTS
cashTicker <- "CASH"

PrintSimulatorState <- function() {
  print(paste("Num rows in PortfoliosInfo:", nrow(PortfoliosInfo)))
  print(paste("Num rows in HoldingsInfo:", nrow(HoldingsInfo)))
  print(paste("Num rows in TransactionsInfo:", nrow(TransactionsInfo)))
  if(nrow(HoldingsInfo) > 0) {
    print(paste("Time Period from", min(HoldingsInfo$ACTIVITY_TS), "to", max(HoldingsInfo$ACTIVITY_TS)))
  }
}