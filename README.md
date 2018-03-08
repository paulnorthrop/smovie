
<!-- README.md is generated from README.Rmd. Please edit that file -->
smovie
======

[![Travis-CI Build Status](https://travis-ci.org/paulnorthrop/smovie.svg?branch=master)](https://travis-ci.org/paulnorthrop/smovie) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/smovie?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/smovie) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/smovie)](https://cran.r-project.org/package=smovie)

### Some Movies to Illustrate Concepts in Statistics

### What does smovie do?

The `smovie` package provides movies to help students to understand statistical concepts. The [`rpanel` package](https://cran.r-project.org/package=rpanel) is used to create interactive plots that move to illustrate key statistical ideas and methods. The movies cover the topics of probability distributions; sampling distributions of the mean (central limit theorem), the maximum (extremal types theorem) and the (Fisher transformation of the) correlation coefficient; simple linear regression; hypothesis testing.

### An example

The function `wws` produces a movie to visualise the Wald, Wilks and score likelihood-based test statistics, for a model with a scalar unknown parameter. The user can change the value of the parameter under a simple null hypothesis and observe the effect on the test statistics. The following code uses the log-likelihood from a binomial experiment and considers the null hypothesis that the success probability *θ* is equal to *θ*<sub>0</sub>. The user may specify their own log-likelihood.

``` r
wws(theta0 = 0.5)
```

### Installation

To get the current released version from CRAN:

``` r
install.packages("smovie")
```

Note that [`rpanel`](https://cran.r-project.org/package=rpanel) requires the Tcl extension [`BWidget`](https://sourceforge.net/projects/tcllib/files/BWidget/). `BWidget` is included in the R installers for Windows and macOS. For other platforms please see [Section 1.1.7 of Writing R Extensions](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Non_002dR-scripts-in-packages) for installation advice.

### Vignettes

See `vignette("smovie-vignette", package = "smovie")` for an overview of the package.
