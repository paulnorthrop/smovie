# smovie 1.1.2

## Dependencies

* The packages `revdbayes` and `SuppDists` have been demoted from Imports to Suggests.  An error is thrown if actions that require either of these packages occurs and that packae is not available.

# smovie 1.1.1

## New features

* New movie: `cltq()`.  Central Limit Theorem for sample quantiles.  Illustrates the sampling distribution of the sample quantiles and the central limit theorem for sample quantiles.

* New movie: `mean_vs_median()`.  Compares the sampling distributions of the mean and median for random samples from either a standard normal distribution or a standard Student t distribution.

* pkgdown documentation at https://paulnorthrop.github.io/smovie/

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
