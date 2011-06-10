\name{StoreTransactionsToDB}
\alias{StoreTransactionsToDB}
\title{
Stores transactions info to database
}
\description{
Stores transactions info in simulator to database
}
\usage{
StoreTransactionsToDB(schemaDbName, dfTransactionsInfo)
}
\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{dfTransactionsInfo}{
Data frame in simulator containing transactions information
}
}

\examples{
## See TestSimulator.R
}

