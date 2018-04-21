# smovie 1.0.0.9000

## Bug fixes and minor improvements

* The way that objects modified inside the plotting function are saved has been changed to enable two or more panels produced by the same smovie function to operate simultaneously and independently, even if the call is exactly the same.

* The argument `plot_par` in the functions `discrete` and `continuous` didn't work properly.  It does now.

* Both `hscale` and `vscale` may be passed to the main menu function `movies` to scale the size of the graphics panel horizontal and vertically.

* In `ett()` and `clt()` the legend position in plots of cdfs has been moved to "topleft" better to avoid overlapping the contents of the plot.  The argument `leg_cex` has been added to enable manual control of the size of the legends.

* That smovie requires the R extension BWidget (via its dependence on the package rpanel) is noted in the README file. If BWidget is not installed then a message is given to explain why the smovie functions don't work.
