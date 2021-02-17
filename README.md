
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quarks

<!-- badges: start -->
<!-- badges: end -->

The goal of `quarks` is to enable the user to compute Value at Risk
(VaR) and Expected Shortfall (ES) by means of various types of
historical simulation. Currently plain historical simulation as well as
age and volatility-weighted historical simulation is implemented in
`quarks`. Volatility weighting is carried out via an exponentially
weighted moving average (EWMA). In future versions of `quarks`
volatility filtering by means of GARCH-type models will be considered.

## Installation

You can install the released version of quarks from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("quarks")
```

## Example 1

This is a basic example which shows you how to solve a common problem.
The data `DAX30` in this package contains daily financial data of the
DAX from 2000 to December 2019 (currency in EUR). In the following
examples the (out-of-sample) one-step forecasts of the 99%-VaR and the
corresponding ES are computed.

``` r
library(quarks)         # Call the package
```

``` r
# Calculating the returns
prices <- DAX30$price.close
returns <- diff(log(prices))

n <- length(returns)
nout <- 250 # number of obs. for out-of-sample forecasting
retout <- returns[(n - nout + 1):n] # out-of-sample returns

### Example 1 - plain historical simulation 
results1 <- rollcast(x = returns, p = 0.99, method = 'plain')
matplot(1:nout, cbind(-retout, results1$VaR, results1$ES),
  type = 'lll',
  xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
  main = 'Plain HS - 99% VaR and ES for the DAX30 return series'
)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
### Example 2 - age weighted historical simulation 
results2 <- rollcast(x = returns, p = 0.99, method = 'age')
matplot(1:nout, cbind(-retout, results2$VaR, results2$ES),
  type = 'lll',
  xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
  main = 'Age weighted HS - 99% VaR and ES for the DAX30 return series'
)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
### Example 3 - volatility weighted historical simulation 
results3 <- rollcast(x = returns, p = 0.99, method = 'vwhs')
matplot(1:nout, cbind(-retout, results3$VaR, results3$ES),
  type = 'lll',
  xlab = 'number of out-of-sample obs.', ylab = 'losses, VaR and ES',
  main = 'Vol. weighted HS - 99% VaR and ES for the DAX30 return series'
)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

To further analyze these results one might apply various backtesting
procedures (e.g. traffic light test) in order to assess the performance
of these methods.
