\name{StoreCorrelationsToDB}
\alias{StoreCorrelationsToDB}
\title{
Stores Correlations info to database
}
\description{
Stores correlations info in simulator to database
}
\usage{
StoreCorrelationsToDB(schemaDbName, dfCorrelationsInfo)
}

\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{dfCorrelationsInfo}{
Data frame in simulator containing portfolios information
}
}

