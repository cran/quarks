#' Rolling one-step forecasts of Value at Risk and Expected Shortfall
#'
#' Computes rolling one-step forecasts of Value at Risk and Expected Shortfall
#' by means of plain historical simulation as well as age- and volatility-
#' weighted historical simulation.
#'
#' @param x a numeric vector of asset returns
#' @param p confidence level for VaR calculation; default is 0.95\%
#' @param method method to be used for calculation; default is 'plain'
#' @param lambda decay factor for the calculation of weights; default is 0.94
#' for \emph{method = 'age'} and 0.98 for \emph{method = 'vwhs'}
#' @param nout number of out-of-sample observations; default is NULL
#' @param nwin window size for rolling one-step forecasting; default is NULL
#'
#' @export
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{VaR}{numerical vector containing out-of-sample forecasts of Value at
#' Risk}
#' \item{ES}{numerical vector containing out-of-sample forecasts of Expected
#' Shortfall}
#' \item{xout}{numerical vector containing out-of-sample returns}
#' }
#' @examples
#'
#' prices <- DAX30$price.close
#' returns <- diff(log(prices))
#' n <- length(returns)
#' nout <- 250 # number of obs. for out-of-sample forecasting
#' retout <- returns[(n - nout + 1):n]
#'
#'
#' ### Example 1 - plain historical simulation
#' results1 <- rollcast(x = returns, p = 0.99, method = 'plain', nout = 250,
#'                      nwin = 500)
#' matplot(1:nout, cbind(-retout, results1$VaR, results1$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Plain HS - 99% VaR and ES for the DAX30 return series'
#' )
#'
#' ### Example 2 - age weighted historical simulation
#' results2 <- rollcast(x = returns, p = 0.99, method = 'age', nout = 250,
#'                      nwin = 500)
#' matplot(1:nout, cbind(-retout, results2$VaR, results2$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Age weighted HS - 99% VaR and ES for the DAX30 return series'
#' )
#'
#' ### Example 3 - volatility weighted historical simulation
#' results3 <- rollcast(x = returns, p = 0.99, method = 'vwhs', nout = 250,
#'                      nwin = 500)
#' matplot(1:nout, cbind(-retout, results3$VaR, results3$ES),
#'   type = 'hll',
#'   xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
#'   main = 'Vol. weighted HS - 99% VaR and ES for the DAX30 return series'
#' )

rollcast <- function(x, p = 0.95, method = c("plain", "age", "vwhs"),
                     lambda = c(0.94, 0.98), nout = NULL, nwin = NULL) {
    if (length(x) <= 1 || !all(!is.na(x)) || !is.numeric(x)) {
        stop("A numeric vector of length > 1 and without NAs must be passed to",
             " 'x'.")
    }
    if (length(p) != 1 || is.na(p) || !is.numeric(p) || (p <= 0)) {
        stop("The argument 'p' must be a single non-NA double value with ",
             "0 < p < l.")
    }
    if (!(length(method) %in% c(1, 3)) || !all(!is.na(method)) ||
        !is.character(method) || !method %in% c("plain", "age", "vwhs")) {
        stop("A single character value must be passed to 'method'.",
             "Valid choices are 'plain', 'age' or 'vwhs'.")
    }
    if (!(length(lambda) %in% c(1, 2)) || !all(!is.na(lambda)) ||
        !is.numeric(lambda) || all(lambda < 0) || all(lambda >= 1)) {
        stop("The argument 'lambda' must be a single non-NA double value with ",
             "0 < lambda < 1.")
    }
    if (length(nout) != 1 || is.na(nout) || !is.numeric(nout) || nout < 0 ||
        is.null(nout)) {
        stop("The argument 'nout' must be a single non-NA integer value.")
    }
    if (length(nwin) != 1 || is.na(nwin) || !is.numeric(nwin) || nwin <= 1 ||
        is.null(nwin)) {
        stop("The argument 'nwin' must be a single non-NA integer value
             with nwin >= 1.")
    }
    if (nwin >= length(x) || nout >= length(x) || (nwin + nout) >= length(x)) {
        stop("Window size and (or) out-of-sample size are too large.")
    }

    if (all(method == c("age", "plain")))
        method <- "plain"
    if (all(lambda == c(0.94, 0.98)) && method == "age")
        lambda <- 0.98
    if (all(lambda == c(0.94, 0.98)) && method == "vwhs")
        lambda <- 0.94

    n <- length(x)
    nin <- n - nout
    xin <- x[1:nin]
    xout <- x[(nin + 1):n]
    xstart <- xin[(nin - nwin + 1):nin]
    fcasts <- matrix(NA, max(nout, 1), 2, dimnames = list(c(), c("VaR", "ES")))
    if (method == "plain") {
        fcasts[1, ] <- hs(xstart, p = p, method = method)
        if (nout > 1) {
            for (i in 2:nout) {
                if (i <= nwin) {
                    fcasts[i, ] <- hs(c(xstart[i:nwin], xout[1:(i - 1)]),
                                      p = p, method = method)
                }
                else{
                    fcasts[i, ] <- hs(xout[(i - nwin):(i - 1)], p = p,
                                      method = method)
                }
            }
        }
    }
    if (method == "age") {
        fcasts[1, ] <- hs(xstart, p = p, method = method, lambda = lambda)
        if (nout > 1) {
            for (i in 2:nout) {
                if (i <= nwin) {
                    fcasts[i, ] <- hs(c(xstart[i:nwin], xout[1:(i - 1)]),
                                      p = p, method = method, lambda = lambda)
                }
                else{
                    fcasts[i, ] <- hs(xout[(i - nwin):(i - 1)], p = p,
                                      method = method, lambda = lambda)
                }
            }
        }
    }
    if (method == "vwhs") {
        fcasts[1, ] <- vwhs(xstart, p = p, lambda = lambda)
        if (nout > 1) {
            for (i in 2:nout) {
                if (i <= nwin) {
                    fcasts[i, ] <- vwhs(c(xstart[i:nwin],
                                          xout[1:(i - 1)]), p = p,
                                        lambda = lambda)
                }
                else{
                    fcasts[i, ] <- vwhs(xout[(i - nwin):(i - 1)], p = p,
                                        lambda = lambda)
                }
            }
        }
    }
    VaR <- fcasts[, 1]
    ES <- fcasts[, 2]
    results <- list(VaR = VaR, ES = ES, xout = xout)

    class(results) <- "quarks"
    attr(results, "function") <- "rollcast"

    results
}
