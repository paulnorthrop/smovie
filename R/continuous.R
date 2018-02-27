# ================================ continuous =================================

#' Univariate Continuous Distributions: p.d.f and c.d.f.
#'
#' A movie to illustrate how the probability density function (p.d.f.) and
#' cumulative distribution function (c.d.f.) of a continuous random variable
#' depends on the values of its parameters.
#'
#' @param distn Either a character string or a function to choose the
#'   continuous random variable.
#'
#'   Strings \code{"beta"}, \code{"cauchy"}, \code{"chisq"}
#'   \code{"chi-squared"}, \code{"exponential"}, \code{"f"}, \code{"gamma"},
#'   \code{"gev"}, \code{"gp"}, \code{"lognormal"}, \code{"log-normal"},
#'   \code{"normal"}, \code{"t"}, \code{"uniform"} and \code{"weibull"} are
#'   recognised, case being ignored.  The relevant distributional functions
#'   \code{dxxx} and \code{pxxx} in the \code{\link[stats]{stats-package}}
#'   are used.  The abbreviations \code{xxx} are also recognised.
#'   The \code{"gev"} and \code{"gp"} cases use the
#'   \code{\link[revdbayes]{gev}} and \code{\link[revdbayes]{gp}}
#'   distributional functions in the \code{\link[revdbayes]{revdbayes}}
#'   package.
#'   If \code{distn = "gamma"} then the \code{(shape, rate)}
#'   parameterisation is used, unless a value for \code{scale} is provided
#'   via the argument \code{params} when the \code{(shape, scale)}
#'   parameterisation is used.
#'
#'   Valid functions are set up like a standard distributional function
#'   \code{dxxx}, with first argument \code{x}, last argument \code{log}
#'   and with arguments to set the parameters of the distribution in between.
#'   See the \href{https://cran.r-project.org/web/views/Distributions.html}{
#'   CRAN task view on distributions}.
#'
#'   If \code{distn} is not supplied then \code{distn = "normal"}
#'   is used.
#' @param var_range A numeric vector of length 2.  Can be used to set a fixed
#'   range of values over which to plot the p.d.f. and c.d.f., in order better
#'   to see the effects of changing the parameter values.
#'   If \code{var_range} is set then it overrides \code{p_vec} (see below).
#' @param params A named list of initial parameter values with which to start
#'   the movie.  If \code{distn} is a string and a particular parameter value
#'   is not supplied then the following values are used.
#'   \code{"normal"}: \code{mean = 0, sd = 1};
#'
#'   If \code{distn} is a function then \code{params} must set any required
#'   parameters.
#'
#'   If parameter value is outside the corresponding range specified by
#'   \code{param_range} then it is set to the closest limit of the range.
#' @param param_step A named list of the amounts by which the respective
#'   parameters in \code{params} are increased/decreased after one click of
#'   the +/- button. If \code{distn} is a function then the default is 0.1
#'   for all parameters.  If \code{distn} is a string then a sensible
#'   distribution-specific default is set internally.
#' @param param_range A named list of the ranges over which the respective
#'   parameters in \code{params} are allowed to vary.  Each element of the list
#'   should be a vector of length 2: the first element gives the lower limit
#'   of the range, the second element the upper limit.
#'   Use \code{NA} to impose no limit.
#'   If \code{distn} is a function then all parameters are unconstrained.
#' @param p_vec A numeric vector of length 2.  The p.d.f. and c.d.f. are
#'   plotted between the 100\code{p_vec[1]}\% and 100\code{p_vec[2]}\%
#'   quantiles of the distribution.  If \code{p_vec} is not supplied then
#'   a sensible distribution-specific default is used.  If \code{distn} is
#'   a function then the default is \code{p_vec = c(0.001, 0.999)}.
#' @param smallest A positive numeric scalar.  The smallest value to be
#'   used for any strictly positive parameters when \code{distn} is a string.
#' @param plot_par A named list of graphical parameters
#'   (see \code{link[graphics]{par}}) to be passed to
#'   \code{\link[graphics]{plot}}.  This may be used to alter the appearance
#'   of the plots of the p.m.f. and c.d.f.
#' @param panel_plot A logical parameter that determines whether the plot
#'   is placed inside the panel (\code{TRUE}) or in the standard graphics
#'   window (\code{FALSE}).  If the plot is to be placed inside the panel
#'   then the tkrplot library is required.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details The movie starts with a plot of the p.d.f. of the distribution
#'   for the initial values of the parameters.  There are buttons to
#'   increase (+) or decrease (-) each parameter.  There is a radio button
#'   to switch the plot from the p.d.f. to the c.d.f. and back.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' # Normal example
#' continuous()
#' # Fix the range of values over which to plot
#' continuous(var_range = c(-10, 10))
#' # The same example, but using a user-supplied function and setting manually
#' # the initial parameters, parameter step size and range
#' continuous(distn = dnorm, params = list(mean = 0, sd = 1),
#'            param_step = list(mean = 1, sd = 1),
#'            param_range = list(sd = c(0, NA)))
#' }
#' @export
continuous <- function(distn, var_range = NULL, params = list(),
                       param_step = list(), param_range = list(),
                       p_vec = NULL, smallest = 0.01, plot_par = list(),
                       panel_plot = TRUE, hscale = NA, vscale = hscale, ...) {
  # To add another distribution
  # 1. misc.R: add code to set_fun_args(), parameter_range(), parameter_step(),
  #            set_p_vec()
  # 2. add lines to dfun, qfun, pfun
  # 3. plot_continuous(): add to the_distn
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  # Smallest value of positive parameters to be set in parameter_range()
  ep <- smallest
  if (!is.list(params)) {
    stop("params must be a named list")
  }
  if (!is.list(plot_par)) {
    stop("plot_par must be a named list")
  }
  if (missing(distn)) {
    distn <- "normal"
  }
  if (is.function(distn)) {
    if (length(params) == 0) {
      stop("params must be supplied")
    }
    dfun <- distn
    user_fun <- as.character(substitute(distn))
    if (user_fun[1] == "::") {
      stop("Use dnorm, not stats::dnorm (for example)")
    }
    root_name <- substr(user_fun, start = 2, stop = nchar(user_fun))
    pfun <- paste("p", root_name, sep = "")
    qfun <- paste("q", root_name, sep = "")
    distn <- "user"
    # Set the arguments to the distributional functions
    fun_args <- set_fun_args(distn, dfun, fun_args, params)
    # Extract the names of the parameters and find the number of parameters
    par_names <- names(fun_args)
    n_pars <- length(par_names)
    # Set the limits on the parameters, the parameter stepsize and the support
    par_range <- parameter_range(distn, fun_args, ep, n_pars)
    par_step <- parameter_step(distn, fun_args, n_pars)
    # Merge the lists, giving precedence to the user-supplied param_step
    par_range <- merge_lists(par_range, param_range)
    par_step <- merge_lists(par_step, param_step)
  } else if (is.character(distn)) {
    distn <- tolower(distn)
    if (distn == "log-normal") {
      distn <- "lognormal"
    }
    if (distn == "chisquared") {
      distn <- "chi-squared"
    }
    # Allow stats:: abbreviations
    distn <- recognise_stats_abbreviations(distn)
    root_name <- distn
    # Set the density, distribution and quantile functions
    #
    dfun <-
      switch(distn,
             "normal" = stats::dnorm,
             "beta" = stats::dbeta,
             "cauchy" = stats::dcauchy,
             "chi-squared" = stats::dchisq,
             "exponential" = stats::dexp,
             "f" = stats::df,
             "gamma" = stats::dgamma,
             "gev" = revdbayes::dgev,
             "gp" = revdbayes::dgp,
             "lognormal" = stats::dlnorm,
             "t" = stats::dt,
             "uniform" = stats::dunif,
             "weibull" = stats::dweibull,
             NULL)
    if (is.null(dfun)) {
      stop("Unsupported distribution")
    }
    pfun <-
      switch(distn,
             "normal" = stats::pnorm,
             "beta" = stats::pbeta,
             "cauchy" = stats::pcauchy,
             "chi-squared" = stats::pchisq,
             "exponential" = stats::pexp,
             "f" = stats::pf,
             "gamma" = stats::pgamma,
             "gev" = revdbayes::pgev,
             "gp" = revdbayes::pgp,
             "lognormal" = stats::plnorm,
             "t" = stats::pt,
             "uniform" = stats::punif,
             "weibull" = stats::pweibull)
    qfun <-
      switch(distn,
             "normal" = stats::qnorm,
             "beta" = stats::qbeta,
             "cauchy" = stats::qcauchy,
             "chi-squared" = stats::qchisq,
             "exponential" = stats::qexp,
             "f" = stats::qf,
             "gamma" = stats::qgamma,
             "gev" = revdbayes::qgev,
             "gp" = revdbayes::qgp,
             "lognormal" = stats::qlnorm,
             "t" = stats::qt,
             "uniform" = stats::qunif,
             "weibull" = stats::qweibull)
    # Set the arguments to the distributional functions
    fun_args <- set_fun_args(distn, dfun, fun_args, params,
                             for_continuous = TRUE)
    # Extract the names of the parameters and find the number of parameters
    par_names <- names(fun_args)
    n_pars <- length(par_names)
    # Set the limits on the parameters, the parameter stepsize and the support
    par_range <- parameter_range(distn, fun_args, ep, n_pars)
    par_step <- parameter_step(distn, fun_args, n_pars)
    # Merge the lists, giving precedence to the user-supplied param_step
    par_range <- merge_lists(par_range, param_range)
    par_step <- merge_lists(par_step, param_step)
  } else {
    stop("distn must be a character scalar or a function")
  }
  # Check that the initial values are in the parameter ranges
  # If not then move the initial value to the closest end of par_range
  for (i in 1:n_pars) {
    par_range_i <- unlist(par_range[i])
    if (!is.na(par_range_i[2]) && fun_args[i] > par_range_i[2]) {
      fun_args[i] <- par_range_i[2]
    } else if (!is.na(par_range_i[1]) && fun_args[i] < par_range_i[1]) {
      fun_args[i] <- par_range_i[1]
    }
  }
  pdf_or_cdf <- "pdf"
  # Temporarily change the name of any argument called size to n,
  # because size is a man argument of rp.control
  if (any(names(fun_args) == "size")) {
    par_names[which(names(fun_args) == "size")] <- "n"
  }
  # If p_vec has not been set then set a distribution-specific default
  if (is.null(p_vec)) {
    p_vec <- set_p_vec(distn)
  }
  # A list of arguments to pass to the plotting function via rp.control()
  pass_args <- fun_args
  names(pass_args) <- par_names
  my_title <- paste("pdf and cdf for the", root_name, "distribution")
  for_rp_control <- c(list(title = my_title,
                           dfun = dfun, pfun = pfun, qfun = qfun,
                           distn = distn, fun_args = fun_args, n_pars = n_pars,
                           par_names = par_names, pdf_or_cdf = pdf_or_cdf,
                           plot_par = plot_par, root_name = root_name,
                           var_range = var_range, p_vec = p_vec),
                      pass_args)
  continuous_panel <- do.call(rpanel::rp.control, for_rp_control)
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
    rpanel::rp.tkrplot(panel = continuous_panel, name = redraw_plot,
                       plotfun  = plot_continuous, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
  } else {
    action <- plot_continuous
  }
  # Produce a double button for each parameter
  call_doublebutton <- function(i) {
    eval(
      substitute(
        rpanel::rp.doublebutton(panel = continuous_panel, variable = x,
                                title = par_names[i], step = par_step[i],
                                action = action, initval = fun_args[i],
                                range = unlist(par_range[i]), ...),
        list(x = as.name(par_names[i]))
      )
    )
    return(invisible())
  }
  for (i in 1:n_pars) {
    call_doublebutton(i)
  }
  rpanel::rp.radiogroup(panel= continuous_panel, pdf_or_cdf, c("pdf", "cdf"),
                        title = "pdf or cdf",
                        action = action)
  rpanel::rp.do(continuous_panel, action = action)
  return(invisible())
}

plot_continuous <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    # Put the parameter values in a named list
    new_fun_args <- list()
    for (i in 1:n_pars) {
      temp <- as.list(eval(as.name(par_names[i])))
      names(temp) <- names(fun_args)[i]
      new_fun_args <- c(new_fun_args, temp)
    }
    distn_name <- distn
    if (distn == "gamma") {
      if (!is.null(fun_args$scale)) {
        distn_name <- "gamma2"
      } else {
        distn_name <- "gamma1"
      }
    }
    if (distn_name == "user") {
      par_paste <- "("
      for (i in 1:n_pars) {
        par_paste <- paste(par_paste, new_fun_args[i])
        if (i < n_pars) {
          par_paste <- paste(par_paste, ",")
        }
      }
      par_paste <- paste(par_paste, ")")
    }
    the_distn <-
      switch(distn_name,
             "normal" = paste(distn, "(", new_fun_args$mean, ",",
                                new_fun_args$sd, ")"),
             "beta" = paste(distn, "(", new_fun_args$shape1, ",",
                            new_fun_args$shape2, ",", new_fun_args$ncp, ")"),
             "cauchy" = paste("Cauchy", "(", new_fun_args$location, ",",
                              new_fun_args$scale, ")"),
             "chi-squared" = paste(distn, "(", new_fun_args$df, ",",
                              new_fun_args$ncp, ")"),
             "exponential" = paste(distn, "(", new_fun_args$rate, ")"),
             "f" = paste("F", "(", new_fun_args$df1, ",", new_fun_args$df2,
                         ",", new_fun_args$ncp, ")"),
             "gamma1" = paste(distn, "(", new_fun_args$shape, ",",
                              new_fun_args$rate, ")"),
             "gamma2" = paste(distn, "(", new_fun_args$shape, ",",
                              new_fun_args$scale, ")"),
             "gev" = paste("GEV", "(", new_fun_args$loc, ",",
                           new_fun_args$scale, ",", new_fun_args$shape, ")"),
             "gp" = paste("GP", "(", new_fun_args$loc, ",",
                          new_fun_args$scale, ",", new_fun_args$shape, ")"),
             "lognormal" = paste(distn, "(", new_fun_args$meanlog, ",",
                                   new_fun_args$sdlog, ")"),
             "t" = paste("Student t", "(", new_fun_args$df, ",",
                                 new_fun_args$ncp, ")"),
             "uniform" = paste(distn, "(", new_fun_args$min, ",",
                               new_fun_args$max, ")"),
             "weibull" = paste("Weibull", "(", new_fun_args$shape, ",",
                               new_fun_args$scale, ")"),
             "user" = paste(root_name, par_paste)
      )
    if (is.null(var_range)) {
      var_range <- variable_range(distn, new_fun_args, qfun, p_vec)
    }
    var_range <- seq(from = var_range[1], to = var_range[2], len = 1001)
    my_col <- "black"
    fun_args <- new_fun_args
    my_xlab <- ifelse(!is.null(plot_par$xlab), plot_par$xlab, "x")
    my_ylab <- ifelse(pdf_or_cdf == "pdf", "density", "probability")
    my_ylab <- ifelse(!is.null(plot_par$ylab), plot_par$ylab, my_ylab)
    my_bty <- ifelse(!is.null(plot_par$bty), plot_par$bty, "l")
    if (pdf_or_cdf == "pdf") {
      probs <- do.call(dfun, c(list(x = var_range), fun_args))
      cond <- is.finite(probs)
      probs <- probs[cond]
      var_range <- var_range[cond]
      my_ylim <- c(0, max(probs))
      my_main <- ifelse(!is.null(plot_par$main), plot_par$main,
                        paste("pdf of the", the_distn, "distribution"))
    } else {
      probs <- do.call(pfun, c(list(q = var_range), fun_args))
      cond <- is.finite(probs)
      probs <- probs[cond]
      var_range <- var_range[cond]
      my_ylim <- c(0, 1)
      my_main <- ifelse(!is.null(plot_par$main), plot_par$main,
                        paste("cdf of the", the_distn, "distribution"))
    }
    if (!is.null(plot_par$col)) {
      my_col <- plot_par$col
    } else {
      my_col <- "black"
    }
    for_plot <- list(x = var_range, y = probs, type = "l", xlab = my_xlab,
                     ylab = my_ylab, col = my_col, bty = my_bty,
                     main = my_main, ylim = my_ylim)
    do.call(graphics::plot, for_plot)
    graphics::par(old_par)
  })
  return(panel)
}
