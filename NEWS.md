# smovie 1.0.0.9000

## Bug fixes and minor improvements

* That smovie requires the RExtension BWidget (via its dependence on the package rpanel) is made explicit using the SystemRequirements field in DESCRIPTION.  If BWidget is not installed then a message is given to explain why the smovie functions don't work.  The README file gives more information.

* The argument `plot_par` in the functions `discrete` and `continuous` didn't work properly.  It does now.
