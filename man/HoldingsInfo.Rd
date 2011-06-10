\name{HoldingsInfo}
\alias{HoldingsInfo}
\docType{data}
\title{
Data frame of asset holding for different portfolios
}
\description{
Data frame contain information on asset holding for different portfolios in the simulator
}
\format{
  A data frame with the following 4 variables.
  \describe{
    \item{\code{PORTFOLIOID}}{a numeric vector representing the Id of Portfolio}
    \item{\code{ACTIVITY_TS}}{timestamp}
    \item{\code{TICKER}}{stock ticker symbol}
    \item{\code{NUMSHARES}}{number of shares held}
  }
}

\examples{
## See TestSimulator.R
}
\keyword{datasets}
