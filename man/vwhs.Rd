% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vwhs.R
\name{vwhs}
\alias{vwhs}
\title{Volatility weighted historical simulation}
\usage{
vwhs(x, p = 0.975, model = c("EWMA", "GARCH"), lambda = 0.94, ...)
}
\arguments{
\item{x}{a numeric vector of asset returns}

\item{p}{confidence level for VaR calculation; default is \code{0.975}}

\item{model}{model for estimating conditional volatility; default is \code{'EWMA'}}

\item{lambda}{decay factor for the calculation of weights; default is \code{0.94}}

\item{...}{additional arguments of the \code{ugarchspec} function from the
\code{rugarch}-package; the default settings for the arguments
\code{variance.model} and \code{mean.model} are \code{list(model = 'sGARCH',
garchOrder = c(1, 1))} and \code{list(armaOrder = c(0, 0))}, respectively}
}
\value{
Returns a list with the following elements:
\describe{
\item{VaR}{Calculated Value at Risk}
\item{ES}{Calculated Expected Shortfall (Conditional Value at Risk)}
\item{p}{Confidence level for VaR calculation}
\item{garchmod}{The model fit. Is the respective GARCH fit for
\code{model = 'GARCH'} (see \code{rugarch} documentation) and  \code{'EWMA'} for
\code{model = 'EWMA'}}
}
}
\description{
Calculates univariate Value at Risk and Expected Shortfall (Conditional
Value at Risk) by means of volatility weighted historical simulation.
Volatility can be estimated with an exponentially weighted moving average
or a GARCH-type model.
}
\examples{
prices <- DAX$price_close
returns <- diff(log(prices))
# volatility weighting via EWMA
ewma <- vwhs(x = returns, p = 0.975, model = "EWMA", lambda = 0.94)
ewma
# volatility weighting via GARCH
garch <- vwhs(x = returns, p = 0.975, model = "GARCH", variance.model =
list(model = "sGARCH"))
garch
}
