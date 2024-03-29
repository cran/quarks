% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DAX.R
\docType{data}
\name{DAX}
\alias{DAX}
\title{German Stock Market Index (DAX) Financial Time Series Data}
\format{
A data frame with 5582 rows and 10 variables:
\describe{
  \item{price.open}{opening price (daily)}
  \item{price.high}{highest price (daily)}
  \item{price.low}{lowest price (daily)}
  \item{price.close}{closing price (daily)}
  \item{volume}{trading volume}
  \item{price.adjusted}{adjusted closing price (daily)}
  \item{ref.date}{date in format YY-MM-DD}
  \item{ticker}{ticker symbol}
  \item{ret.adjusted.prices}{returns obtained from the adj. closing prices}
  \item{ret.closing.prices}{returns obtained from the closing prices}
}
}
\source{
The data was obtained from Yahoo Finance.
}
\usage{
DAX
}
\description{
A dataset that contains the daily financial data of the DAX from
2000 to December 2021 (currency in EUR).
}
\keyword{datasets}
