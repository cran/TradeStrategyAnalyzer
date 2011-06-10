\name{StoreSimulationResults}
\alias{StoreSimulationResults}
\title{
Stores simulation results to database
}
\description{
Stores simulation results such PortfoliosInfo, HoldingsInfo, TransactionsInfo to database
}
\usage{
StoreSimulationResults(schemaDbName, PortfoliosInfo, HoldingsInfo, TransactionsInfo)
}
\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{PortfoliosInfo}{
Data frame in simulator containing portfolios information
}
  \item{HoldingsInfo}{
Data frame in simulator containing asset holdings information
}
  \item{TransactionsInfo}{
Data frame in simulator containing transactions information
}
}

\examples{
## See TestSimulator.R
}
