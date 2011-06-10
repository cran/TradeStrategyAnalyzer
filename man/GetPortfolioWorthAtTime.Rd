\name{GetPortfolioWorthAtTime}
\alias{GetPortfolioWorthAtTime}
\title{
Computes the net value of a portfolio at a particular time
}
\description{
This function computes the net value of a portfolio at a particular instance in time.
}
\usage{
GetPortfolioWorthAtTime(timestamp, portfolioId, StockMarketDataset, HoldingsInfo)
}
\arguments{
  \item{timestamp}{
Instance in time
}
  \item{portfolioId}{
Id of portfolio
}
  \item{StockMarketDataset}{
Data frame containing stock market dataset
}
  \item{HoldingsInfo}{
Data frame contain information on asset holding for different portfolios in the simulator
}
}

\value{
Net value in dollars of the Portfolio at the specified time
}

\examples{
## See TestSimulator.R
}

