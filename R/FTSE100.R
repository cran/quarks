#' Financial Times Stock Exchange Index (FTSE) Financial Time Series Data
#'
#' A dataset that contains the daily financial data of the FTSE from
#' 2000 to December 2021 (currency in EUR).
#'
#' @format A data frame with 5558 rows and 10 variables:
#' \describe{
#'   \item{price.open}{opening price (daily)}
#'   \item{price.high}{highest price (daily)}
#'   \item{price.low}{lowest price (daily)}
#'   \item{price.close}{closing price (daily)}
#'   \item{volume}{trading volume}
#'   \item{price.adjusted}{adjusted closing price (daily)}
#'   \item{ref.date}{date in format YY-MM-DD}
#'   \item{ticker}{ticker symbol}
#'   \item{ret.adjusted.prices}{returns obtained from the adj. closing prices}
#'   \item{ret.closing.prices}{returns obtained from the closing prices}
#' }
#' @source The data was obtained from Yahoo Finance.
"FTSE100"
