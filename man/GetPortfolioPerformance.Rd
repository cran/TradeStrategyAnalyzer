\name{GetPortfolioPerformance}
\alias{GetPortfolioPerformance}
\title{
Computes Portfolio Return
}
\description{
This function computes the portfolio return based on the time period of the stock market dataset. It returns the profit/loss on the final asset value over the initial asset value.
}
\usage{
GetPortfolioPerformance(portfolioId, PortfoliosInfo, HoldingsInfo, StockMarketDataset)
}
\arguments{
  \item{portfolioId}{
Id of portfolio
}
  \item{PortfoliosInfo}{
Data frame containing information on portfolios in simulator
}
  \item{HoldingsInfo}{
Data frame contain information on asset holding for different portfolios in the simulator
}
  \item{StockMarketDataset}{
Data frame containing stock market dataset
}
}

\value{
Overall Portfolio Return in Percentage
}

\examples{
## See TestSimulator.R
}
