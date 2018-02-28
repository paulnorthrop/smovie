# ================================= discrete ==================================

#' Univariate Discrete Distributions: p.m.f and c.d.f.
#'
#' A movie to illustrate how the probability mass function (p.m.f.) and
#' cumulative distribution function (c.d.f.) of a discrete random variable
#' depends on the values of its parameters.
#'
#' @param distn Either a character string or a function to choose the discrete
#'   random variable.
#'
#'   Strings \code{"binomial"}, \code{"geometric"},
#'   \code{"hypergeometric"}, \code{"negative binomial"} and \code{"poisson"}
#'   are recognised, case being ignored.  The relevant distributional functions
#'   \code{dxxx} and \code{pxxx} in the \code{\link[stats]{stats-package}}
#'   are used.  The abbreviations \code{xxx} are also recognised.
#'   If \code{distn = "hypergeometric"} then the \code{(size, prob)}
#'   parameterisation is used, unless a value for \code{mu} is provided
#'   via the argument \code{params} when the \code{(size, mu)}
#'   parameterisation is used.
#'
#'   Valid functions are set up like a standard distributional function
#'   \code{dxxx}, with first argument \code{x}, last argument \code{log}
#'   and with arguments to set the parameters of the distribution in between.
#'   See the \href{https://cran.r-project.org/web/views/Distributions.html}{
#'   CRAN task view on distributions}.
#'   It is assumed that the support of the random variable is a
#'   subset of the integers, unless \code{var_support} is set to the contrary.
#'
#'   If \code{distn} is not supplied then \code{distn = "binomial"}
#'   is used.
#' @param var_support A numeric vector.  Can be used to set a fixed set of
#'   values for which to plot the p.m.f. and c.d.f., in order better
#'   to see the effects of changing the parameter values or to set a support
#'   that isn't a subset of the integers.
#'   If \code{var_support} is set then it overrides \code{p_vec} (see below).
#' @param params A named list of initial parameter values with which to start
#'   the movie.  If \code{distn} is a string and a particular parameter value
#'   is not supplied then the following values are used.
#'   \code{"binomial"}: \code{size = 10, prob = 0.5};
#'   \code{"geometric"}: \code{prob = 0.5};
#'   \code{"hypergeometric"}: \code{m = 10, n = 7, k = 8};
#'   \code{"negative binomial"}: \code{size = 10, prob = 0.5};
#'   \code{"poisson"}: \code{lambda = 5}.
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
#' @param observed_value A non-negative integer.  If \code{observed_value} is
#'   supplied then the corresponding line in the plot of the p.m.f. is coloured
#'   in red.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details The movie starts with a plot of the p.m.f. of the distribution
#'   for the initial values of the parameters.  There are buttons to
#'   increase (+) or decrease (-) each parameter.  There is a radio button
#'   to switch the plot from the p.m.f. to the c.d.f. and back.
#'
#'   Owing to a conflict with the argument \code{size} of the function
#'   \code{\link[rpanel]{rp.control}} the parameter \code{size} of,
#'   for example, the binomial and negative binomial distributions, is
#'   labelled as \code{n}.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' # Binomial example
#' discrete()
#'
#' # The same example, but using a user-supplied function and setting manually
#' # the initial parameters, parameter step size and range
#' discrete(distn = dbinom, params = list(size = 10, prob = 0.5),
#'          param_step = list(size = 1),
#'          param_range = list(size = c(1, NA), prob = c(0, 1)))
#'
#' # Poisson distribution. Show the use of var_support
#' discrete(distn = "poisson", var_support = 0:20)
#' }
#' @export
discrete <- function(distn, var_support = NULL, params = list(),
                     param_step = list(), param_range = list(), p_vec = NULL,
                     smallest = 0.01, plot_par = list(), panel_plot = TRUE,
                     hscale = NA, vscale = hscale, observed_value = NA, ...) {
  # To add another distribution
  # 1. misc.R: add code to set_fun_args(), parameter_range(), parameter_step(),
  #            variable_support()
  # 2. add lines to dfun, qfun, pfun
  # 3. plot_discrete(): add to the_distn
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
    distn <- "binomial"
  }
  if (is.function(distn)) {
    if (length(params) == 0) {
      stop("params must be supplied")
    }
    dfun <- distn
    user_fun <- as.character(substitute(distn))
    if (user_fun[1] == "::") {
      stop("Use dbinom, not stats::dbinom (for example)")
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
    # Allow stats:: abbreviations
    distn <- recognise_stats_abbreviations(distn)
    root_name <- distn
    # Set the density, distribution and quantile functions
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
  pmf_or_cdf <- "pmf"
  # Temporarily change the name of the binomial or negative binomial size
  # (or an argument size to a user-supplied function) to n, because size
  # is a main argument of rp.control
  if (any(names(fun_args) == "size")) {
    par_names[which(names(fun_args) == "size")] <- "n"
  }
  # If p_vec has not been set then set a distribution-specific default
  if (is.null(p_vec)) {
    p_vec <- c(0.001, 0.999)
  }
  # A list of arguments to pass to the plotting function via rp.control()
  pass_args <- fun_args
  names(pass_args) <- par_names
  my_title <- paste("pmf and cdf of the", root_name, "distribution")
  for_rp_control <- c(list(title = my_title,
                           dfun = dfun, pfun = pfun, qfun = qfun,
                           distn = distn, fun_args = fun_args, n_pars = n_pars,
                           par_names = par_names, pmf_or_cdf = pmf_or_cdf,
                           observed_value = observed_value,
                           plot_par = plot_par, root_name = root_name,
                           var_support = var_support, p_vec = p_vec),
                      pass_args)
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
                                range = unlist(par_range[i]), showvalue = TRUE,
                                ...),
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
    distn_name <- distn
    if (distn == "negative binomial") {
      if (!is.null(fun_args$mu)) {
        distn_name <- "negbin2"
      } else {
        distn_name <- "negbin1"
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
             "binomial" = paste(distn, "(", new_fun_args$size, ",",
                                new_fun_args$prob, ")"),
             "geometric" = paste(distn, "(", new_fun_args$prob, ")"),
             "hypergeometric" = paste(distn, "(", new_fun_args$m, ",",
                                      new_fun_args$n, ",", new_fun_args$k,
                                      ")"),
             "negbin1" = paste(distn, "(", new_fun_args$size, ",",
                                new_fun_args$prob, ")"),
             "negbin2" = paste(distn, "(", new_fun_args$size, ",",
                               new_fun_args$mu, ")"),
             "poisson" = paste("Poisson", "(", new_fun_args$lambda, ")"),
             "user" = paste(root_name, par_paste)
      )
    if (is.null(var_support)) {
      var_support <- variable_support(distn, new_fun_args, qfun, pmf_or_cdf,
                                      p_vec)
    }
    if (is.null(observed_value)) {
      my_col <- "black"
    } else {
      my_col <- rep(1, length(var_support))
      which_obs <- which(var_support == observed_value)
      if (length(which_obs) == 1) {
        my_col[which_obs] <- "red"
      }
    }
    fun_args <- new_fun_args
    my_xlab <- ifelse(!is.null(plot_par$xlab), plot_par$xlab, "x")
    my_ylab <- ifelse(!is.null(plot_par$ylab), plot_par$ylab, "probability")
    my_bty <- ifelse(!is.null(plot_par$bty), plot_par$bty, "l")
    if (pmf_or_cdf == "pmf") {
      probs <- do.call(dfun, c(list(x = var_support), fun_args))
      my_ylim <- c(0, max(probs))
      my_main <- ifelse(!is.null(plot_par$main), plot_par$main,
                        paste("pmf of the", the_distn, "distribution"))
      if (!is.null(plot_par$col)) {
        my_col <- plot_par$col
      }
      for_plot <- list(x = var_support, y = probs, type = "h", xlab = my_xlab,
                       ylab = my_ylab, col = my_col, bty = my_bty,
                       main = my_main, ylim = my_ylim)
      do.call(graphics::plot, for_plot)
    } else {
      probs <- do.call(pfun, c(list(q = var_support), fun_args))
      rval <- stats::approxfun(var_support, probs, method = "constant",
                               yleft = 0, yright = 1, f = 0, ties = "ordered")
      class(rval) <- c("ecdf", "stepfun", class(rval))
      my_main <- ifelse(!is.null(plot_par$main), plot_par$main,
                        paste("cdf of the", the_distn, "distribution"))
      if (!is.null(plot_par$col)) {
        my_col <- plot_par$col
      } else {
        my_col <- "black"
      }
      for_plot <- list(x = rval, xlab = my_xlab, ylab = my_ylab, col = my_col,
                       bty = my_bty, main = my_main)
      do.call(graphics::plot, for_plot)
    }
    graphics::par(old_par)
  })
  return(panel)
}
