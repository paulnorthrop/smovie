# add delta_params ?
# add prob to set range.

# ================================= discrete ==================================

#' Univariate Discrete Distributions: p.m.f and c.d.f.
#'
#' A movie to illustrate how the probability mass function (p.m.f.) and
#' cumulative distribution function (c.d.f.) of a discrete random variable
#' depends on the values of its parameters.
#'
#' @param distn A character scalar specifying the distribution from which
#'   observations are sampled..   Distributions \code{"binomial"},
#'   \code{"geometric"}, \code{"hypergeometric"}, \code{"negative binomial"},
#'   \code{Poisson} are recognised, case being ignored.
#'   The relevant distributional functions \code{dxxx} and \code{pxxx} in the
#'   \code{\link[stats]{stats-package}} is used to .
#'
#'   If \code{distn} is not supplied then \code{distn = "binomial"}
#'   is used.
#' @param params A named list of additional arguments to be passed to the
#'   density function associated with distribution \code{distn}.
#'
#'   If a parameter value is not supplied then the default values in the
#'   relevant distributional function set using \code{distn} are used
#' @param panel_plot A logical parameter that determines whether the plot
#'   is placed inside the panel (\code{TRUE}) or in the standard graphics
#'   window (\code{FALSE}).  If the plot is to be placed inside the panel
#'   then the tkrplot library is required.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @param observed_value A non-negative integer.  If \code{observed_value} is
#'   supplied then the corresponding line in the plot of the p.m.f. is coloured
#'   in red.  If \code{observed_value} is not an integer then
#'   \code{round(observed_value)} is used.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details Ass details.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' discrete()
#' }
#' @export
discrete <- function(distn, params = list(), panel_plot = TRUE, hscale = NA,
                     vscale = hscale, observed_value = NA, ...) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  if (!is.na(observed_value) && observed_value < 0) {
    stop("observed_value cannot be negative")
  }
  observed_value <- round(observed_value)
  if (!is.list(params)) {
    stop("params must be a named list")
  }
  if (missing(distn)) {
    distn <- "binomial"
  }
  # Set the density and quantile functions and simulation function
  # "rngev" is included because it is an example that is in the domain of
  # attraction of the Gumbel case but the upper endpoint is finite.
  #
  dfun <-
    switch(distn,
           "binomial" = stats::dbinom,
           "geometric" = stats::dgeom,
           "hypergeometric" = stats::dhyper,
           "negative binomial" = stats::dnbinom,
           "poisson" = stats::dpois,
           NULL)
  if (is.null(dfun)) {
    stop("Unsupported distribution")
  }
  pfun <-
    switch(distn,
           "binomial" = stats::pbinom,
           "geometric" = stats::pgeom,
           "hypergeometric" = stats::phyper,
           "negative binomial" = stats::pnbinom,
           "poisson" = stats::ppois)
  qfun <-
    switch(distn,
           "binomial" = stats::qbinom,
           "geometric" = stats::qgeom,
           "hypergeometric" = stats::qhyper,
           "negative binomial" = stats::qnbinom,
           "poisson" = stats::qpois)
  # Set the arguments to the distributional functions
  fun_args <- set_fun_args(distn, dfun, fun_args, params)
  # Extract the names of the parameters and find the number of parameters
  par_names <- names(fun_args)
  n_pars <- length(par_names)
  # Set the limits on the parameters, the parameter stepsize and the support
  par_range <- parameter_range(distn)
  par_step <- parameter_step(distn)
  #
  pmf_or_cdf <- "pmf"
  # Temporarily change the name of the binomial size to n, because size
  # is a man argument of rp.control
  if (distn == "binomial") {
    par_names[1] <- "n"
  }
  # A list of arguments to pass to the plotting function via rp.control()
  pass_args <- fun_args
  names(pass_args) <- par_names
  my_title <- paste("pmf and cdf for the", distn, "distribution")
  for_rp_control <- c(list(title = my_title,
                           dfun = dfun, pfun = pfun, qfun = qfun,
                           distn = distn, fun_args = fun_args, n_pars = n_pars,
                           par_names = par_names, pmf_or_cdf = pmf_or_cdf,
                           observed_value = observed_value), pass_args)
  discrete_panel <- do.call(rpanel::rp.control, for_rp_control)
  redraw_plot <- NULL
  panel_redraw <- function(panel) {
    rpanel::rp.tkrreplot(panel = panel, name = redraw_plot)
    return(panel)
  }
  if (panel_plot & !requireNamespace("tkrplot", quietly = TRUE)) {
    warning("tkrplot is not available so panel_plot has been set to FALSE.")
    panel_plot <- FALSE
  }
  if (panel_plot) {
    rpanel::rp.tkrplot(panel = discrete_panel, name = redraw_plot,
                       plotfun  = plot_discrete, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
  } else {
    action <- plot_discrete
  }
  # Produce a double button for each parameter
  call_doublebutton <- function(i) {
    eval(
      substitute(
        rpanel::rp.doublebutton(panel = discrete_panel, variable = x,
                                title = par_names[i], step = par_step[i],
                                action = action, initval = fun_args[i],
                                range = par_range[, i], ...),
        list(x = as.name(par_names[i]))
      )
    )
    return(invisible())
  }
  for (i in 1:n_pars) {
    call_doublebutton(i)
  }
  rpanel::rp.radiogroup(panel= discrete_panel, pmf_or_cdf, c("pmf", "cdf"),
                        title = "pmf or cdf",
                        action = action)
  rpanel::rp.do(discrete_panel, action = action)
  return(invisible())
}

plot_discrete <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    # Put the parameter values in a named list
    new_fun_args <- list()
    for (i in 1:n_pars) {
      temp <- as.list(eval(as.name(par_names[i])))
      names(temp) <- names(fun_args)[i]
      new_fun_args <- c(new_fun_args, temp)
    }
    the_distn <-
      switch(distn,
             "binomial" = paste(distn, "(", new_fun_args$size, ",",
                                new_fun_args$prob, ")"),
             "geometric" = paste(distn, "(", new_fun_args$prob, ")"),
             "poisson" = paste(distn, "(", new_fun_args$lambda, ")")
      )
    var_support <- variable_support(distn, new_fun_args, qfun)
    if (is.null(observed_value)) {
      my_col <- 1
    } else {
      my_col <- rep(1, length(var_support))
      which_obs <- which(var_support == observed_value)
      if (length(which_obs) == 1) {
        my_col[which_obs] <- 2
      }
    }
    fun_args <- new_fun_args
    if (pmf_or_cdf == "pmf") {
      probs <- do.call(dfun, c(list(x = var_support), fun_args))
      graphics::plot(var_support, probs, type = "h", axes = FALSE,
                     ylab = "pmf", xlab = "", col = my_col)
      graphics::axis(1, at = var_support, labels = var_support, cex.axis = 0.7)
      graphics::axis(2, las = 1)
      graphics::title(main = paste("pmf of the", the_distn, "distribution"))
      graphics::box(bty = "l")
    } else {
      probs <- do.call(pfun, c(list(q = var_support), fun_args))
      rval <- stats::approxfun(var_support, probs, method = "constant",
                               yleft = 0, yright = 1, f = 0, ties = "ordered")
      class(rval) <- c("ecdf", "stepfun", class(rval))
      graphics::plot(rval, main = paste("cdf of the", the_distn,
                                        "distribution"))
    }
    graphics::par(old_par)
  })
  return(panel)
}
