\name{TransactionsInfo}
\alias{TransactionsInfo}
\docType{data}
\title{
Data frame contain information on transactions
}
\description{
Data frame containing information on transactions in simulator
}
\format{
  A data frame with the following 5 variables.
  \describe{
    \item{\code{PORTFOLIOID}}{Id of Portfolio}
    \item{\code{ACTIVITY_TS}}{timestamp}
    \item{\code{DECISION}}{type of decision: "BUY", "SELL"}
    \item{\code{TICKER}}{stock ticker symbol}
    \item{\code{NUMSHARES}}{number of shares}
  }
}

\examples{
## See TestSimulator.R
}
\keyword{datasets}
