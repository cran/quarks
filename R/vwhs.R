#' Volatility weighted historical simulation
#'
#' Calculates univariate Value at Risk and Expected Shortfall by means
#' of volatility weighted historical simulation. Volatility is
#' estimated with an exponentially weighted moving average.
#'
#' @param x a numeric vector of asset returns
#' @param p confidence level for VaR calculation; default is 0.95\%
#' @param lambda decay factor for the calculation of weights; default is 0.94
#'
#' @export
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{VaR}{Calculated Value at Risk}
#' \item{ES}{Calculated Expected Shortfall}
#' }
#' @examples
#' prices <- DAX30$price.close
#' returns <- diff(log(prices))
#' vwhs(x = returns, p = 0.95, lambda = 0.94)

vwhs <- function(x, p = 0.95, lambda = 0.94) {
    if (length(x) <= 1 || !all(!is.na(x)) || !is.numeric(x)) {
        stop("A numeric vector of length > 1 and without NAs must be passed to",
             " 'x'.")
    }
    if (length(p) != 1 || is.na(p) || !is.numeric(p) || (p <= 0)) {
        stop("The argument 'p' must be a single non-NA double value with ",
             "0 < p < l.")
    }
    if (length(lambda) != 1 || is.na(lambda) || !is.numeric(lambda) ||
        lambda < 0 || lambda >= 1) {
        stop("The argument 'lambda' must be a single non-NA double value with ",
             "0 < lambda < 1.")
    }
    n <- length(x)
    cvar <- ewma(x, lambda = lambda)
    csig <- sqrt(cvar)
    xz <- x/csig
    loss <- -(xz * csig[n])
    VaR <- stats::quantile(loss, p)
    ES <- mean(loss[loss >= VaR])
    results <- cbind(VaR = VaR, ES = ES)
    colnames(results) <- c("VaR", "ES")
    rownames(results) <- paste0(100 * p, "%")
    results
}
