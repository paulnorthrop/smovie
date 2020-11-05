# smovie 1.1.3.9000

# smovie 1.1.3

## UCL Eugenics Inquiry

Explicit references to (Karl) Pearson have removed from the package following the [Inquiry into the History of Eugenics at UCL](https://www.ucl.ac.uk/provost/inquiry-history-eugenics-ucl).

## Bug fixes and minor improvements

* Errors in test name strings supplied to `testthat::test_that()` have been corrected, to avoid CRAN package check ERRORs.

* In `wws()` the gradient of the log-likelihood was unnecessarily labelled with a red "score" when the null value of theta0 is less than the MLE of theta.  This has been removed.

* In `wws()`, when `model = "norm"` the value of `mu` could not be passed as described.  This has been corrected.

* In `shypo()` there was a bug that meant that the plots did not behave as intended when `mu0` is not equal to (the default value of) 0.  This has been corrected.

* In the documentation for `shypo()` a minor typo at the end of the Details section has been corrected: "based on the current value of n" has been deleted.

* In `discrete()` radio buttons have been added to switch between the version of the the geometric distribution based on the number of trials up to including the first success and the number of failures until the first success.

* In `discrete()` a typo has been corrected in the 4th line of the documentation of the argument `distn`: "hypergeometric" should have read "negative binomial".

# smovie 1.1.2

## Dependencies

* The packages `revdbayes` and `SuppDists` have been demoted from Imports to Suggests.  An error is thrown if actions that require either of these packages occurs and that package is not available.

# smovie 1.1.1

## New features

* New movie: `cltq()`.  Central Limit Theorem for sample quantiles.  Illustrates the sampling distribution of the sample quantiles and the central limit theorem for sample quantiles.

* New movie: `mean_vs_median()`.  Compares the sampling distributions of the mean and median for random samples from either a standard normal distribution or a standard Student t distribution.

* pkgdown documentation at [https://paulnorthrop.github.io/smovie/](https://paulnorthrop.github.io/smovie/).

## Bug fixes and minor improvements

* In `ett()` the sample size `n` is now allowed to drop to 1 for those cases in which the normalising constants underlying the calculations are finite, such as the exponential, uniform and generalised Pareto distributions.  Otherwise, 2 is the smallest allowable value for `n`.

* When the Wald, Wilks and Score tests movie `wws()` is called from the main menu, via `movies()` a binomial example is used, so that the differences between the test statistics can be seen.

* Adjustments have been made to the `wws()` movie to ensure that the plot stays completely still during the animations.

# smovie 1.0.1

## Bug fixes and minor improvements

* The way that objects modified inside the plotting function are saved has been changed to enable two or more panels produced by the same smovie function to operate simultaneously and independently, even if the call is exactly the same.

* The argument `plot_par` in the functions `discrete` and `continuous` didn't work properly.  It does now.

* Both `hscale` and `vscale` may be passed to the main menu function `movies` to scale the size of the graphics panel horizontal and vertically.

* In `ett()` and `clt()` the legend position in plots of cdfs has been moved to "topleft" better to avoid overlapping the contents of the plot.  The argument `leg_cex` has been added to enable manual control of the size of the legends.

* That smovie requires the R extension BWidget (via its dependence on the package rpanel) is noted in the README file. If BWidget is not installed then a message is given to explain why the smovie functions don't work.
