\name{AddPortfolio}
\alias{AddPortfolio}
\title{
Creates a new portfolio
}
\description{
This function creates a new portfolio in the strategy simulator. Returns data frame of portfolios that includes this newly created portfolio.
}
\usage{
AddPortfolio(PortfoliosInfo, portfolioId, algorithm, startAmount)
}

\arguments{
  \item{PortfoliosInfo}{
Data frame for storing information on portfolios in simulator
}
  \item{portfolioId}{
Portfolio Id
}
  \item{algorithm}{
Name for this Portfolio
}
  \item{startAmount}{
Initial investment amount in dollars
}
}

\examples{
## see TestSimulator.R
}

