#' Volatility weighted historical simulation
#'
#' Calculates univariate Value at Risk and Expected Shortfall (also called
#' Conditional Value at Risk) by means of volatility weighted historical
#' simulation. Volatility can be estimated with an exponentially weighted
#' moving average or a GARCH-type model.
#'
#' @param x a numeric vector of asset returns
#' @param p confidence level for VaR calculation; default is 0.975
#' @param model model for estimating conditional volatility; default is 'EWMA'
#' @param lambda decay factor for the calculation of weights; default is 0.94
#' @param ... additional arguments of the \emph{ugarchspec} function from the
#' \emph{rugarch}-package; the default settings for the arguments
#' \emph{variance.model} and \emph{mean.model} are \emph{list(model = 'sGARCH',
#' garchOrder = c(1, 1))} and \emph{list(armaOrder = c(0, 0))}, respectively
#'
#' @export
#'
#' @return Returns a list with the following elements:
#' \describe{
#' \item{VaR}{Calculated Value at Risk}
#' \item{ES}{Calculated Expected Shortfall (Conditional Value at Risk)}
#' \item{garchmod}{The model fit. Is the respective GARCH fit for
#' \emph{model = "GARCH"} (see \emph{rugarch} documentation) and  'EWMA' for
#' \emph{model = "EWMA"}}
#' }
#' @examples
#' prices <- DAX30$price.close
#' returns <- diff(log(prices))
#' # volatility weighting via EWMA
#' ewma <- vwhs(x = returns, p = 0.975, model = "EWMA", lambda = 0.94)
#' ewma$VaR_ES
#' # volatility weighting via GARCH
#' garch <- vwhs(x = returns, p = 0.975, model = "GARCH", variance.model =
#' list(model = "sGARCH"))
#' garch$VaR_ES

vwhs <- function(x, p = 0.975, model = c("EWMA", "GARCH"), lambda = 0.94, ...)
    {
    if (length(x) <= 1 || !all(!is.na(x)) || !is.numeric(x)) {
        stop("A numeric vector of length > 1 and without NAs must be passed to",
             " 'x'.")
    }
    if (length(p) != 1 || is.na(p) || !is.numeric(p) || (p <= 0)) {
        stop("The argument 'p' must be a single non-NA double value with ",
             "0 < p < l.")
    }
    if (!length(model) %in% c(1, 2) || !all(!is.na(model)) ||
        !is.character(model) || !all(model %in% c("EWMA", "GARCH"))) {
        stop("A single character value must be passed to 'model'.",
             "Valid choices are 'EWMA' or 'GARCH'.")
    }
    if (length(lambda) != 1 || is.na(lambda) || !is.numeric(lambda) ||
        lambda < 0 || lambda >= 1) {
        stop("The argument 'lambda' must be a single non-NA double value with ",
             "0 < lambda < 1.")
    }
    if (all(model == c("EWMA", "GARCH")))
        model <- "EWMA"
    dots <- list(...)
    if (!"mean.model" %in% names(dots)) {
        dots[["mean.model"]] <- list(armaOrder = c(0, 0))
    }
    if (model == "GARCH") {
        spec <- do.call(what = rugarch::ugarchspec, args = dots)
        fit <- rugarch::ugarchfit(data = x, spec = spec)
        csig <- as.numeric(rugarch::sigma(fit))
    }
    if (model == "EWMA") {
        cvar <- ewma(x, lambda = lambda)
        csig <- sqrt(cvar)
        fit <- "EWMA"
    }
    n <- length(x)
    xz <- x/csig
    loss <- -(xz * csig[n])
    VaR <- stats::quantile(loss, p)
    ES <- mean(loss[loss > VaR])
    results <- cbind(VaR = VaR, ES = ES)
    colnames(results) <- c("VaR", "ES")
    rownames(results) <- paste0(100 * p, "%")
    results <- list(VaR_ES = results, garchmod = fit)
    results
}
