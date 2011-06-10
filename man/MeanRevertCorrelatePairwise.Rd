\name{MeanRevertCorrelatePairwise}
\alias{MeanRevertCorrelatePairwise}
\title{
Computes correlation information on pair of stocks
}
\description{
This function computes correlation coefficient for all stock pairs in the stock market dataset and returns the ones that are above the specified correlation thresholds. The results are sorted with highest correlation values at the top. Currently, this function uses the Pearson method on the stock price time-series to compute correlations. This approach has limitations and other techniques may also be used to compute correlations. This is meant to be an illustrative example.
}
\usage{
MeanRevertCorrelatePairwise(StockMarketDataset, corrThreshold = 0.95)
}
\arguments{
  \item{StockMarketDataset}{
Data frame containing stock market dataset
}
  \item{corrThreshold}{
Correlation Thresholds for returned result
}
}

\value{
Returns a data frame of top correlated stock pairs. The dataframe has the following attributes for each row/stock pair:
"TICKER1", "TICKER2", "CORR", "ABSCORR", "MEANDIFF", "SDEVDIFF"
}


\examples{
## See TestSimulator.R
}

