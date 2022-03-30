#' Unconditional and Conditional Coverage Tests, Independence Test
#'
#' The conditional (Kupiec, 1995), the unconditional coverage
#' test (Christoffersen, 1998) and the independence test (Christoffersen, 1998)
#' of the Value-at-Risk (VaR) are applied.
#'
#' @param obj a list that contains the following elements:
#' \describe{
#' \item{\code{loss}}{a numeric vector that contains the values of a loss series
#' ordered from past to present; is set to \code{NULL} by default.}
#' \item{\code{VaR}}{a numeric vector that contains the estimated values of the
#' VaR for the same time points of the loss series \code{loss};
#' is set to \code{NULL} by default.}
#' \item{\code{p}}{a numeric vector with one element; defines the probability p
#' stated in the null hypotheses of the coverage tests (see the section
#' \code{Details} for more information); is set to \code{NULL} by default.}
#' }
#' Please note that a list returned by the \code{rollcast} function can be directly
#' passed to \code{cvgtest}.
#'
#' @export
#'
#' @importFrom stats 'pchisq'
#'
#' @details
#' The function needs three inputs: the out-of-sample loss series \code{obj$loss}, the
#' corresponding estimated \code{obj$VaR} series and the coverage level \code{obj$p},
#' for which the VaR has been calculated. If an object returned by this function
#' is entered into the R console, a detailed overview of the test
#' results is printed.
#'
#' @return
#' A list of class \code{quarks} with the following four elements:
#' \describe{
#' \item{p}{probability p stated in the null hypotheses of the coverage tests}
#' \item{p.uc}{the p-value of the unconditional coverage test}
#' \item{p.cc}{the p-value of the conditional coverage test}
#' \item{p.ind}{the p-value of the independence test}
#' \item{model}{selected model for estimation; only available if a list
#' returned by the \code{rollcast}) is passed to \code{cvgtest}}
#' \item{method}{selected method for estimation; only available if a list
#' returned by the \code{rollcast}) is passed to \code{cvgtest}}
#' }
#'
#' @references
#' Christoffersen, P. F. (1998). Evaluating interval forecasts. International
#' economic review, pp. 841-862.
#'
#' Kupiec, P. (1995). Techniques for verifying the accuracy of risk measurement
#' models. The J. of Derivatives, 3(2).
#'
#'
#' '@details
#' With this function, the conditional and the unconditional coverage
#' tests introduced by Kupiec (1995) and Christoffersen (1998) can be applied.
#' Given a return series \eqn{r_t} with \eqn{n} observations, divide the
#' series into \eqn{n-K} in-sample and \eqn{K} out-of-sample observations,
#' fit a model to the in-sample data and obtain rolling one-step forecasts of
#' the VaR for the out-of-sample time points.
#'
#' Define
#'
#' \deqn{I_t = 1,}
#'
#' if \eqn{-r_t > \widehat{VaR}_t (\alpha)} or
#'
#' \deqn{I_t = 0,}
#'
#' otherwise,
#' for \eqn{t = n + 1, n + 2, ..., n + K} as the hit sequence, where \eqn{\alpha} is
#' the confidence level for the VaR (often \eqn{\alpha = 0.95} or \eqn{\alpha = 0.99}).
#' Furthermore, denote \eqn{p = \alpha} and let \eqn{w} be the actual covered
#' proportion of losses in the data.
#'
#' @examples
#'
#' prices <- DAX$price.close
#' returns <- diff(log(prices))
#' n <- length(returns)
#' nout <- 250 # number of obs. for out-of-sample forecasting
#' nwin <- 500 # window size for rolling forecasts
#' results <- rollcast(x = returns, p = 0.975, method = 'age', nout = nout,
#'                      nwin = nwin)
#' cvgtest(results)
#'

cvgtest <- function(obj = list(loss = NULL, VaR = NULL, p = NULL)) {
  if (!is.list(obj) && !is.data.frame(obj)) {
    stop("A list or data frame containing two vectors with equal
         length and without NAs as well as a single numeric value
         must be passed to", " 'obj'.")
  }

  if (!inherits(obj, "quarks") && (length(obj[["loss"]]) <= 1 ||
                                   !all(!is.na(obj[["loss"]])) ||
                                   !is.numeric(obj[["loss"]]))) {
    stop("A numeric vector of length > 1 and without NAs must be passed to",
         " 'obj[['loss']]'.")
  }

  if (!inherits(obj, "quarks") && (length(obj[["VaR"]]) <= 1 ||
                                   !all(!is.na(obj[["VaR"]])) ||
                                   !is.numeric(obj[["VaR"]]))) {
    stop("A numeric vector of length > 1 and without NAs must be passed to",
         " 'obj[['VaR']]'.")
  }

  if (!inherits(obj, "quarks") && (length(obj[["p"]]) != 1 ||
                                   is.na(obj[["p"]]) ||
                                   !is.numeric(obj[["p"]]) ||
                                   obj[["p"]] <= 0 || obj[["p"]] >= 1)) {
    stop("A single numeric value that satisfies >0 and <1 must be passed to",
         " 'obj[['p']]'")
  }

  if(inherits(obj, "quarks")) {
    loss <- -obj[["xout"]]
    VaR <- obj[["VaR"]]
    p <- obj[["p"]]
    model <- obj[["model"]]
    method <- obj[["method"]]
  }
  else {
    loss <- obj[["loss"]]
    VaR <- obj[["VaR"]]
    if (is.null(model)) model <- NA
    if (is.null(method)) method <- NA
  }

  n.out <- length(loss)
  It <- loss > VaR
  n0 <- sum(1 - It[2:n.out])
  n1 <- sum(It[2:n.out])
  pe <- n1/(n0 + n1)
  Tp <- n0 * log(p) + n1 * log(1 - p)
  T1 <- n0 * log(1 - pe) + n1 * log(pe)
  LRuc <- -2 * (Tp - T1)

  Itf <- It[1:(n.out - 1)]
  Its <- It[2:n.out]
  diff.It <- Itf - Its
  diff.It.0 <- diff.It == 0
  n00 <- sum(diff.It.0[Itf == 0])
  n01 <- sum(diff.It == -1)
  n10 <- sum(diff.It == 1)
  n11 <- sum(diff.It.0[Itf == 1])

  p01 <- n01 / (n00 + n01)
  p11 <- n11 / (n10 + n11)

  if (n11 == 0) {
    T2 <- n00 * log(1 - p01) + n01 * log(p01)
  } else {
    T2 <- n00 * log(1 - p01) + n01 * log(p01) + n10 * log(1 - p11) + n11 * log(p11)
  }
  LRind <- -2 * (T1 - T2)

  p.uc <- 1 - pchisq(LRuc, 1)
  p.ind <- 1 - pchisq(LRind, 1)

  ########## correct conditional coverage##############
  num.cc <- n0 * log(p) + n1 * log(1 - p)
  if (n11 == 0) {
    denom.cc <- n00 * log(1 - p01) + n01 * log(p01)
  } else {
    denom.cc <- n00 * log(1 - p01) + n01 * log(p01) + n10 * log(1 - p11) + n11 * log(p11)
  }

  LR.cc <- -2 * (num.cc - denom.cc)
  p.cc <- 1 - stats::pchisq(LR.cc, 2)

  result <- list(p = p,
                 p.uc = p.uc,
                 p.ind = p.ind,
                 p.cc = p.cc,
                 method = method,
                 model = model)
  class(result) <- "quarks"
  attr(result, "function") <- "cvgtest"
  result
}
