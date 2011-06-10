\name{StorePortfoliosToDB}
\alias{StorePortfoliosToDB}
\title{
Stores portfolios info to database
}
\description{
Stores portfolios info in simulator to database
}
\usage{
StorePortfoliosToDB(schemaDbName, dfPortfoliosInfo)
}

\arguments{
  \item{schemaDbName}{
The file path to the .db sqlite database file. The sqlite database should include tables as specified in create_tables.sql included in the package.
}
  \item{dfPortfoliosInfo}{
Data frame in simulator containing portfolios information
}
}

\examples{
## See TestSimulator.R
}

