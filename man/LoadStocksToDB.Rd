\name{LoadStocksToDB}
\alias{LoadStocksToDB}
\title{
Loads information on individual stocks to R and database
}
\description{
This function reads in a .csv file containing individual stock's information into the R environment as well as storing the information in a database table.
}
\usage{
LoadStocksToDB(schemaDbName, inputStockInfoFile)
}
\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{inputStockInfoFile}{
The input comma-delimited file containing information on individual daily stock prices. Each row contains TICKER, COMPANY, INDUSTRY. See SP500industry.csv in the package for an example.
}
}

\value{
Returns a data frame containing individual stock price info (ACTIVITY_TS, TICKER, PRICE, VOLUME).
}

\examples{
## See TestSimulator.R
}

