\name{VisualizeCorrelation}
\alias{VisualizeCorrelation}
\title{
Visualize the correlation of stock pairs
}
\description{
This function allows one to plot the stock price time-series of a pair of stocks identified as correlated in the algorithm.
}
\usage{
VisualizeCorrelation(pairCorrelationInfo, StockMarketDataset, topPosition)
}
\arguments{
  \item{pairCorrelationInfo}{
Data frame containing pairs of stocks and their pairwise correlation info
}
  \item{StockMarketDataset}{
Data frame containing stock market data
}
  \item{topPosition}{
The top N-th correlated stock pair to be displayed.
}
}

