#' Print Method for the Package 'quarks'
#'
#'This function regulates how objects created by the package \code{quarks} are
#'printed.
#'
#'@param x an input object of class \code{quarks}.
#'@param ... included for compatibility; additional arguments will however
#'not affect the output.
#'
#'@export
#'
#'@return
#'None

print.quarks <- function(x, ...) {
  if(attr(x, "function") == "rollcast") {
    VaR <- x[["VaR"]]
    ES <- x[["ES"]]
    loss <- -x[["xout"]]
    p <- x[["p"]]
    It.var <- ifelse(VaR <= loss, 1, 0)
    br.var <- sum(It.var)
    It.es <- ifelse(ES <= loss, 1, 0)
    br.es <- sum(It.es)
    if (x[["method"]] == "plain") {
      method = "Plain"
    }
    if (x[["method"]] == "age") {
      method = "Age Weighting"
    }
    if (x[["method"]] == "vwhs") {
      method = "Volatility Weighting"
    }
    if (x[["method"]] == "fhs") {
      method = "Filtered"
    }
    cat("--------------------------------------------", fill = TRUE)
    cat("|              Specifications              |", fill = TRUE)
    cat("--------------------------------------------", fill = TRUE)
    cat(" Out-of-sample size:   ", x[["nout"]], fill = TRUE)
    cat(" Rolling window size:  ", x[["nwin"]], fill = TRUE)
    cat(" Bootstrap sample size:", x[["nboot"]], fill = TRUE)
    cat(" Confidence level:     ", paste(p * 100, "%"), fill = TRUE)
    cat(" Method:               ", method, fill = TRUE)
    cat(" Model:                ", x[["model"]], fill = TRUE)
    cat("--------------------------------------------", fill = TRUE)
    cat("|           Forecast performance           |", fill = TRUE)
    cat("--------------------------------------------", fill = TRUE)
    cat(" Out-of-sample losses exceeding VaR", fill = TRUE)
    cat(" ", fill = TRUE)
    cat(" Number of breaches:   ", br.var, fill = TRUE)
    cat("--------------------------------------------", fill = TRUE)
    cat(" Out-of-sample losses exceeding ES", fill = TRUE)
    cat(" ", fill = TRUE)
    cat(" Number of breaches:   ", br.es, fill = TRUE)
    cat("--------------------------------------------", fill = TRUE)
  }
}
