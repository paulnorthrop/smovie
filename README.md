
<!-- README.md is generated from README.Rmd. Please edit that file -->
smovie
======

[![Travis-CI Build Status](https://travis-ci.org/paulnorthrop/smovie.svg?branch=master)](https://travis-ci.org/paulnorthrop/smovie) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/smovie?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/smovie)

### Some movies to teach statistical concepts

### What smovie do?

This package is in the very early stages of development.

The 'rpanel' package <https://cran.r-project.org/package=rpanel> is used to create interactive plots that move to illustrate key statistical ideas and methods.

### An example

The function `wws` produces a movie to visualise the Wald, Wilks and score likelihood-based test statistics, for a model with a scalar unknown parameter. The user can change the value of the parameter under a simple null hypothesis and observe the effect on the test statistics. The following code uses the log-likelihood from a binomial experiment.

``` r
wws(theta0 = 0.5)
```

### Installation

To install this development version from Github:

``` r
install.packages("devtools")
devtools::install_github("paulnorthrop/smovie")
```
