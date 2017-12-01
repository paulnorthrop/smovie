
<!-- README.md is generated from README.Rmd. Please edit that file -->
smovie
======

### Some movies to teach statistical concepts

### What smovie do?

This package is in the very early stages of development.

The 'rpanel' package <https://cran.r-project.org/package=rpanel> is used to create interactive plots that move to illustrate key statistical ideas and methods.

### An example

The function `wws` produces a movie to visualise the Wald, Wilks and score likelihood-based test statistics, for a model with a scalar unknown parameter. The user can change the value of the parameter under a simple null hypothesis and observe the effect on the test statistics. The following code uses the log-likelihood from a binomial experiment.

``` r
bin_loglik <- function(p, n_success, n_failure) {
  return(n_success * log(p) + n_failure * log(1 - p))
}
wws(bin_loglik, theta0 = 0.5, theta_range = c(0.1, 0.7),
    theta_mle = 7 / 20, n_success = 7, n_failure = 13)
```

### Installation

To install this development version from Github:

``` r
install.packages("devtools")
devtools::install_github("paulnorthrop/smovie")
```
