
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smovie

[![R-CMD-check](https://github.com/paulnorthrop/smovie/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paulnorthrop/smovie/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/smovie)](https://cran.r-project.org/package=smovie)
[![Downloads
(monthly)](https://cranlogs.r-pkg.org/badges/smovie?color=brightgreen)](https://cran.r-project.org/package=smovie)
[![Downloads
(total)](https://cranlogs.r-pkg.org/badges/grand-total/smovie?color=brightgreen)](https://cran.r-project.org/package=smovie)

### Some Movies to Illustrate Concepts in Statistics

### What does smovie do?

The `smovie` package provides movies to help students to understand
statistical concepts. The [`rpanel`
package](https://cran.r-project.org/package=rpanel) is used to create
interactive plots that move to illustrate key statistical ideas and
methods. The movies cover the topics of probability distributions;
sampling distributions of the mean (central limit theorem), the median,
the maximum (extremal types theorem) and the (Fisher transformation of
the) correlation coefficient; simple linear regression; hypothesis
testing.

### An example

The function `wws` produces a movie to visualise the Wald, Wilks and
score likelihood-based test statistics, for a model with a scalar
unknown parameter. The user can change the value of the parameter under
a simple null hypothesis and observe the effect on the test statistics.
The following code uses the log-likelihood from a binomial experiment
and considers the null hypothesis that the success probability $\theta$
is equal to $\theta_0$. The user may specify their own log-likelihood.

``` r
wws(theta0 = 0.5)
```

### Installation

To get the current released version from CRAN:

``` r
install.packages("smovie")
```

### Vignettes

See `vignette("smovie-vignette", package = "smovie")` for an overview of
the package.
