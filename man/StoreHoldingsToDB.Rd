\name{StoreHoldingsToDB}
\alias{StoreHoldingsToDB}
\title{
Stores asset holdings info to database
}
\description{
Stores asset holdings info in simulator to database
}
\usage{
StoreHoldingsToDB(schemaDbName, dfHoldingsInfo)
}

\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{dfHoldingsInfo}{
Data frame in simulator containing asset holdings information
}
}


\examples{
## See TestSimulator.R
}