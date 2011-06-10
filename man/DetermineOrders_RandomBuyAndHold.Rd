\name{DetermineOrders_RandomBuyAndHold}
\alias{DetermineOrders_RandomBuyAndHold}
\title{
Generate Order Book using Random Buy-and-Hold Trading Algorithm
}
\description{
This function generates a data frame of trade orders that result from running the random buy-and-hold algorithm.
}
\usage{
DetermineOrders_RandomBuyAndHold(portfolioId, PortfoliosInfo, StockMarketDataset, numCandidates=10)
}
\arguments{
  \item{portfolioId}{
Id of portfolio on which to apply mean revert algorithm
}
  \item{PortfoliosInfo}{
Data frame containing information on portfolios in simulator
}
  \item{StockMarketDataset}{
Data frame containing stock market dataset
}
  \item{numCandidates}{
Number of stocks to buy and hold
}
}

\value{
Returns a data frame containing trade orders. The attributes for each row / trade order are:
"ACTIVITY_TS", "PORTFOLIOID", "TICKER", "NUMSHARES", "ORDERTYPE", "ORDERVALUE"
}

\examples{
## See TestSimulator.R
}

