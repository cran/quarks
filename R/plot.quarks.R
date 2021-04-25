#' Plot Method for the Package 'quarks'
#'
#' This function regulates how objects created by the package quarks are
#' plotted.
#'
#' @param x an input object of class \emph{quarks}.
#' @param ... additional arguments of the standard plot method.
#'
#' @export
#'
#' @return
#' None

plot.quarks <- function(x, ...) {
  dots <- list(...)

  if(attr(x, "function") == "rollcast") {
    dots[["x"]] <- seq_along(x[["xout"]])
    VaR.plot <- ifelse(-x[["xout"]] >= x[["VaR"]], x[["VaR"]], NA)
    ES.plot <- ifelse(-x[["xout"]] >= x[["ES"]], x[["ES"]], NA)

    dots[["y"]] <- cbind(-x[["xout"]], x[["VaR"]], x[["ES"]], VaR.plot, ES.plot)
    dots[["col"]] <- c("darkgrey", "red", "green", "blue", "black")
    dots[["type"]] <- "hllpp"
    dots[["lty"]] <- c(1, 5, 5)
    dots[["pch"]] <- 1
    dots[["xlab"]] <- "out-of-sample observations"
    dots[["ylab"]] <- "Negative returns, VaR and ES"
    dots[["main"]] <- "VaR (red) and ES (green) - (rolling) one-step forecasts"

    do.call(what = graphics::matplot, args = dots)
  }
}

