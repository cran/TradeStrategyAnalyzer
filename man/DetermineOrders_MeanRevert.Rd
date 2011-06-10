\name{DetermineOrders_MeanRevert}
\alias{DetermineOrders_MeanRevert}
\title{
Generate Order Book using Mean Revert Trading Algorithm
}
\description{
This function generates a data frame of trade orders that result from running the mean revert trading algorithm.
}
\usage{
DetermineOrders_MeanRevert(portfolioId, PortfoliosInfo, StockMarketDataset, numPairs = 1)
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
  \item{numPairs}{
Number of trading pairs to be considered in the algorithm
}
}
\value{
Returns a data frame containing trade orders. The attributes for each row / trade order are:
"ACTIVITY_TS", "PORTFOLIOID", "TICKER", "NUMSHARES", "ORDERTYPE", "ORDERVALUE"
}
\examples{
## See TestSimulator.R
}

