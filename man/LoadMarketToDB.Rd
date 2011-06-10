\name{LoadMarketToDB}
\alias{LoadMarketToDB}
\title{
Loads stock market daily prices from csv file to R and database
}
\description{
This function reads in a .csv file containing individual daily stock prices into the R environment as well as storing the information in a database table.
}
\usage{
LoadMarketToDB(schemaDbName, inputMarketInfoFile)
}
\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{inputMarketInfoFile}{
The input comma-delimited file containing information on individual stocks. Each row contains ACTIVITY_TS, TICKER, PRICE, VOLUME. See SP500market.csv in the package for an example.
}
}

\value{
Returns a data frame containing individual dailly stock price info (ACTIVITY_TS, TICKER, PRICE, VOLUME).
}

\examples{
## See TestSimulator.R
}

