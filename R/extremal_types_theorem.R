# =================================== ett =====================================

#' Extremal Types Theorem (ETT)
#'
#' A movie to illustrate the extremal types theorem, that is, convergence
#' of the distribution of the maximum of a random sample of size \eqn{n}
#' from certain distributions to a member of the Generalized Extreme Value
#' (GEV) family, as \eqn{n} tends to infinity.
#' Samples of size \eqn{n} are simulated repeatedly from the chosen
#' distribution.  The distributions (simulated empirical and true) of the
#' sample maxima are compared to the relevant GEV limit.
#'
#' @param n An integer scalar.  The size of the samples drawn from the
#'   distribution chosen using \code{distn}.  \code{n} must be no smaller
#'   than 2.
#' @param distn A character scalar specifying the distribution from which
#'   observations are sampled.   Distributions \code{"beta"},
#'   \code{"cauchy"}, \code{"chisq"}, \code{"chi-squared"},
#'   \code{"exponential"}, \code{"f"}, \code{"gamma"}, \code{"gp"},
#'   \code{lognormal}, \code{log-normal},  \code{"ngev"}, \code{"normal"},
#'   \code{"t"}, \code{"uniform"} and \code{"weibull"} are recognised, case
#'   being ignored.
#'
#'   If \code{distn} is not supplied then \code{distn = "exponential"}
#'   is used.
#'
#'   The \code{"gp"} case uses the \code{\link[revdbayes]{gp}}
#'   distributional functions in the
#'   \code{\link[revdbayes]{revdbayes}} package.
#'
#'   The \code{"ngev"} case is a negated GEV(1 / \eqn{\xi}, 1, \eqn{\xi})
#'   distribution, for \eqn{\xi} > 0, and uses the \code{\link[revdbayes]{gev}}
#'   distributional functions in the
#'   \code{\link[revdbayes]{revdbayes}} package.
#'   If \eqn{\xi} = 1 then this coincides with Example 1.7.5 in Leadbetter,
#'   Lindgren and Rootzen (1983).
#'
#'   The other cases use the distributional functions in the
#'   \code{\link[stats]{stats-package}}.
#'   If \code{distn = "gamma"} then the \code{(shape, rate)}
#'   parameterisation is used.  If \code{scale} is supplied via \code{params}
#'   then \code{rate} is inferred from this.
#'   If \code{distn = "beta"} then \code{ncp} is forced to be zero.
#' @param params A named list of additional arguments to be passed to the
#'   density function associated with distribution \code{distn}.
#'   The \code{(shape, rate)} parameterisation is used for the gamma
#'   distribution (see \code{\link[stats]{GammaDist}}) even if the value of
#'   the \code{scale} parameter is set using \code{params}.
#'
#'   If a parameter value is not supplied then the default values in the
#'   relevant distributional function set using \code{distn} are used,
#'   except for
#'   \code{"beta"} (\code{shape1 = 2, shape2 = 2}),
#'   \code{"chisq"} (\code{df = 4}),
#'   \code{"f"} (\code{df1 = 4, df2 = 8}),
#'   \code{"ngev"} (\code{shape = 0.2}).
#'   \code{"gamma"} (\code{shape = 2},
#'   \code{"gp"} (\code{shape = 0.1}),
#'   \code{"t"} (\code{df = 4}) and
#'   \code{"weibull"} (\code{shape = 2}).
#' @param panel_plot A logical parameter that determines whether the plot
#'   is placed inside the panel (\code{TRUE}) or in the standard graphics
#'   window (\code{FALSE}).  If the plot is to be placed inside the panel
#'   then the tkrplot library is required.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @param n_add An integer scalar.  The number of simulated datasets to add
#'   to each new frame of the movie.
#' @param delta_n A numeric scalar.  The amount by which n is increased
#'   (or decreased) after one click of the + (or -) button in the parameter
#'   window.
#' @param arrow  A logical scalar.  Should an arrow be included to show the
#'   simulated sample maximum from the top plot being placed into the
#'   bottom plot?
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @param ... Additional arguments to the rpanel functions
#'   \code{\link[rpanel]{rp.button}} and
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details Loosely speaking, a consequence of the
#'   \href{https://en.wikipedia.org/wiki/Extreme_value_theory#Univariate_theory}{Extremal Types Theorem}
#'   is that, in many situations, the maximum of a \emph{large number}
#'   \eqn{n} of independent random variables has \emph{approximately} a
#'   GEV(\eqn{\mu, \sigma, \xi)}) distribution, where \eqn{\mu} is a location
#'   parameter, \eqn{\sigma} is a scale parameter and \eqn{\xi} is a shape
#'   parameter.  See Coles (2001) for an introductory account and
#'   Leadbetter et al (1983) for greater detail and more examples.
#'   The Extremal Types Theorem is an asymptotic result that considers the
#'   possible limiting distribution of linearly normalised maxima
#'   as \eqn{n} tends to infinity.
#'   This movie considers examples where this limiting result holds and
#'   illustrates graphically the closeness of the limiting approximation
#'   provided by the relevant GEV limit to the true finite-\eqn{n}
#'   distribution.
#'
#'   Samples of size \code{n} are repeatedly simulated from the distribution
#'   chosen using \code{distn}.  These samples are summarized using a histogram
#'   that appears at the top of the movie screen.  For each sample the maximum
#'   of these \code{n} values is calculated, stored and added to another plot.
#'   This plot is either a histogram or an empirical c.d.f., chosen using a
#'   radio button.
#'
#'   The probability density function (p.d.f.) of the original
#'   variables is superimposed on the top histogram.
#'   There is a checkbox to add to the bottom plot the exact p.d.f./c.d.f. of
#'   the sample maxima and an approximate (large \code{n}) GEV p.d.f./c.d.f.
#'   implied by the ETT.
#'   The GEV shape parameter \eqn{\xi} that applies in the limiting
#'   case is used.  The GEV location \eqn{\mu} and scale
#'   \eqn{\sigma} are set based on constants used to normalise the maxima
#'   to achieve the GEV limit.
#'   Specifically, \eqn{\mu} is set at the 100(1-1/\eqn{n})\% quantile of the
#'   distribution \code{distn} and \eqn{\sigma} at
#'   (1 / \eqn{n}) / \eqn{f(\mu)}, where \eqn{f} is the
#'   density function of the distribution \code{distn}.
#'
#'   Once it starts, four aspects of this movie are controlled by the user.
#'   \itemize{
#'     \item{}{There are buttons to increase (+) or decrease (-) the sample
#'       size, that is, the number of values over which a maximum is
#'       calculated.}
#'     \item{}{Each time the button labelled "simulate another \code{n_add}
#'       samples of size n" is clicked \code{n_add} new samples are simulated
#'       and their sample maxima are added to the bottom histogram.}
#'     \item{}{There is a button to switch the bottom plot from displaying
#'       a histogram of the simulated maxima, the exact p.d.f. and the
#'       limiting GEV p.d.f. to the empirical c.d.f. of the simulated data,
#'       the exact c.d.f. and the limiting GEV c.d.f.}
#'     \item{}{There is a box that can be used to display only the bottom
#'       plot.  This option is selected automatically if the sample size
#'       \eqn{n} exceeds 100000.}
#'     \item{}{There is a box that can be used to display only the bottom
#'       plot.  This option is selected automatically if the sample size
#'       \eqn{n} exceeds 100000.}
#'   }
#'   For further detail about the examples specified by \code{distn}
#'   see Chapter 1 of Leadbetter et al. (1983) and Chapter 3 of
#'   Coles (2001).  In many of these examples
#'   (\code{"exponential", "normal", "gamma", "lognormal", "chi-squared",
#'   "weibull", "ngev"}) the limiting GEV distribution has a shape
#'   parameter that is equal to 0.  In the \code{"uniform"} case the limiting
#'   shape parameter is -1 and in the \code{"beta"} case it is
#'   -1 / \code{shape2}, where \code{shape2} is the
#'   second parameter of the \code{\link[stats]{Beta}} distribution.
#'   In the other cases the limiting shape parameter is positive,
#'   with respective values \code{shape}
#'   (\code{"gp"}, see \code{\link[revdbayes]{gp}}),
#'   1 / \code{df} (\code{"t"}, see \code{\link[stats]{TDist}}),
#'   1 (\code{"cauchy"}, see \code{\link[stats]{Cauchy}}),
#'   2 / \code{df2} (\code{"f"}, see \code{\link[stats]{FDist}}).
#' @return Nothing is returned, only the animation is produced.
#' @references Coles, S. G. (2001) \emph{An Introduction to Statistical
#'   Modeling of Extreme Values}, Springer-Verlag, London.
#'   \url{http://dx.doi.org/10.1007/978-1-4471-3675-0_3}
#' @references Leadbetter, M., Lindgren, G. and Rootzen, H. (1983)
#'   \emph{Extremes and Related Properties of Random Sequences and Processes.}
#'   Springer-Verlag, New York.
#'   \url{http://dx.doi.org/10.1007/978-1-4612-5449-2}
#' @seealso \code{\link{movies}}: a user-friendly menu panel.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' # Check that BWidget is available (system requirement for rpanel)
#' got_BWidget <- suppressWarnings(tclRequire("BWidget"))
#' if (!is.logical(got_BWidget)) {
#'   # Exponential data: xi = 0
#'   ett()
#'
#'   # Uniform data: xi =-1
#'   ett(distn = "uniform")
#'
#'   # Student t data: xi = 1 / df
#'   ett(distn = "t", params = list(df = 5))
#' }
#' @export
ett <- function(n = 20, distn, params = list(), panel_plot = TRUE, hscale = NA,
                vscale = hscale, n_add = 1, delta_n = 1, arrow = TRUE,
                pos = 1, envir = as.environment(pos), ...) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  # To add another distribution
  # 1. misc.R: add code to set_fun_args(), set_top_range(), set_leg_pos()
  # 2. add lines to rfun, dfun, qfun, pfun
  # 3. ett_movie_plot(): add to the_distn and gev_pars
  if (!is.wholenumber(n) | n < 2) {
    stop("n must be an integer that is no smaller than 2")
  }
  if (!is.wholenumber(n_add) | n_add < 1) {
    stop("n_add must be an integer that is no smaller than 1")
  }
  if (!is.wholenumber(delta_n) | delta_n < 1) {
    stop("delta_n must be an integer that is no smaller than 1")
  }
  if (!is.list(params)) {
    stop("params must be a named list")
  }
  if (missing(distn)) {
    distn <- "exponential"
  }
  if (distn == "ngev") {
    if (!is.null(params$loc) | !is.null(params$scale)) {
      warning("In the negated GEV case you cannot set loc or scale")
    }
    if (!is.null(params$shape)){
      if (params$shape <= 0) {
        stop("the shape parameter must be positive in the negated GEV case")
      }
    }
  }
  xlab <- "x"
  #
  distn <- tolower(distn)
  if (distn == "log-normal") {
    distn <- "lognormal"
  }
  if (distn == "chisq") {
    distn <- "chi-squared"
  }
  # Set the density, distribution, quantile and simulation functions
  # "rngev" is included because it is an example that is in the domain of
  # attraction of the Gumbel case but the upper endpoint is finite.
  #
  rfun <-
    switch(distn,
           "exponential" = stats::rexp,
           "uniform" = stats::runif,
           "gp" = revdbayes::rgp,
           "normal" = stats::rnorm,
           "beta" = stats::rbeta,
           "t" = stats::rt,
           "gamma" = stats::rgamma,
           "lognormal" = stats::rlnorm,
           "cauchy" = stats::rcauchy,
           "chi-squared" = stats::rchisq,
           "f" = stats::rf,
           "weibull" = stats::rweibull,
           "ngev" = rngev,
           NULL)
  if (is.null(rfun)) {
    stop("Unsupported distribution")
  }
  dfun <-
    switch(distn,
           "exponential" = stats::dexp,
           "uniform" = stats::dunif,
           "gp" = revdbayes::dgp,
           "normal" = stats::dnorm,
           "beta" = stats::dbeta,
           "t" = stats::dt,
           "gamma" = stats::dgamma,
           "lognormal" = stats::dlnorm,
           "cauchy" = stats::dcauchy,
           "chi-squared" = stats::dchisq,
           "f" = stats::df,
           "weibull" = stats::dweibull,
           "ngev" = dngev)
  qfun <-
    switch(distn,
           "exponential" = stats::qexp,
           "uniform" = stats::qunif,
           "gp" = revdbayes::qgp,
           "normal" = stats::qnorm,
           "beta" = stats::qbeta,
           "t" = stats::qt,
           "gamma" = stats::qgamma,
           "lognormal" = stats::qlnorm,
           "cauchy" = stats::qcauchy,
           "chi-squared" = stats::qchisq,
           "f" = stats::qf,
           "weibull" = stats::qweibull,
           "ngev" = qngev)
  pfun <-
    switch(distn,
           "exponential" = stats::pexp,
           "uniform" = stats::punif,
           "gp" = revdbayes::pgp,
           "normal" = stats::pnorm,
           "beta" = stats::pbeta,
           "t" = stats::pt,
           "gamma" = stats::pgamma,
           "lognormal" = stats::plnorm,
           "cauchy" = stats::pcauchy,
           "chi-squared" = stats::pchisq,
           "f" = stats::pf,
           "weibull" = stats::pweibull,
           "ngev" = pngev)
  # Set the arguments to the distributional functions
  fun_args <- set_fun_args(distn, dfun, fun_args, params)
  # Set sensible scales for the plots
  if (distn == "t") {
    if (fun_args$df < 2) {
      top_p_vec <- c(0.05, 0.95)
      bottom_p_vec <- c(0.01, 0.7)
    } else if (fun_args$df < 3) {
      top_p_vec <- c(0.01, 0.99)
      bottom_p_vec <- c(0.01, 0.9)
    } else {
      top_p_vec <- c(0.001, 0.999)
      bottom_p_vec <- c(0.001, 0.999)
    }
  } else if (distn == "cauchy"){
    top_p_vec <- c(0.05, 0.95)
    bottom_p_vec <- c(0.01, 0.7)
  } else if (distn == "gp") {
    if (fun_args$shape > 0.3) {
      top_p_vec <- c(0.001, 0.95)
      bottom_p_vec <- c(0.001, 0.7)
    } else {
      top_p_vec <- c(0.001, 0.999)
      bottom_p_vec <- c(0.001, 0.999)
    }
  } else {
    top_p_vec <- c(0.001, 0.999)
    bottom_p_vec <- c(0.001, 0.999)
  }
  # Set the range for the top plot
  top_range <- set_top_range(distn, p_vec = top_p_vec, fun_args, qfun)
  # Set the legend position
  leg_pos <- set_leg_pos(distn, fun_args)
  top_leg_pos <- leg_pos$top_leg_pos
  bottom_leg_pos <- leg_pos$bottom_leg_pos
  # Assign variables to an environment so that they can be accessed inside
  # clt_exponential_movie_plot()
  old_n <- 0
  assign("old_n", old_n, envir = envir)
  # Create buttons for movie
  show_dens <- FALSE
  show_dens_only <- FALSE
  pdf_or_cdf <- "pdf"
  assign("old_pdf_or_cdf", pdf_or_cdf, envir = envir)
  assign("old_show_dens", show_dens, envir = envir)
  assign("old_show_dens_only", show_dens_only, envir = envir)
  ett_panel <- rpanel::rp.control("extremal types theorem", n = n,
                                  n_add = n_add, dfun = dfun, qfun = qfun,
                                  rfun = rfun, pfun = pfun,
                                  fun_args = fun_args, distn = distn,
                                  top_range = top_range, top_p_vec = top_p_vec,
                                  bottom_p_vec = bottom_p_vec,
                                  show_dens = FALSE, show_dens_only = FALSE,
                                  pdf_or_cdf = "pdf",
                                  top_leg_pos = top_leg_pos,
                                  bottom_leg_pos = bottom_leg_pos,
                                  xlab = xlab, arrow = arrow, envir = envir)
  #
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
    # Set a seed and then reset it before the panel is redrawn so that only
    # one arrow and sample maximum appears in the first plot
    my_seed <- round(stats::runif(1, 0, 1000))
    set.seed(my_seed)
    rpanel::rp.tkrplot(panel = ett_panel, name = redraw_plot,
                       plotfun = ett_movie_plot, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
    set.seed(my_seed)
  } else {
    action <- ett_movie_plot
  }
  #
  rpanel::rp.doublebutton(panel = ett_panel, variable = n, step = delta_n,
                          title = "sample size, n",
                          action = action, initval = n,
                          range = c(2, NA), showvalue = TRUE, ...)
  if (n_add == 1) {
    my_title <- paste("simulate another sample")
  } else {
    my_title <- paste("simulate another", n_add, "samples")
  }
  dlist <- list(...)
  # If the user hasn't set either repeatdelay or repeatinterval then set them
  # to the default values in rp.doublebutton (100 milliseconds)
  if (is.null(dlist$repeatdelay) & is.null(dlist$repeatinterval)) {
      rpanel::rp.button(panel = ett_panel, action = action, title = my_title,
                        repeatdelay = 100, repeatinterval = 100, ...)
  } else {
    rpanel::rp.button(panel = ett_panel, action = action, title = my_title,
                      ...)
  }
  rpanel::rp.radiogroup(panel= ett_panel, pdf_or_cdf, c("pdf", "cdf"),
                        title = "pdf or cdf in bottom plot",
                        action = action)
  rpanel::rp.checkbox(panel = ett_panel, show_dens,
                      labels = "show exact and GEV pdf/cdf",
                      action = action)
  rpanel::rp.checkbox(panel = ett_panel, show_dens_only,
                      labels = "show only exact and GEV pdf/cdf",
                      action = action)
  rpanel::rp.do(panel = ett_panel, action = action)
  return(invisible())
}

# Function to be called by ett().

ett_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    # Don't simulate very large samples (only show pdfs or cdfs)
    if (n > 100000) {
      show_dens_only <- TRUE
    }
    # Don't add the rug in the top plot if n is large
    if (n > 1000) {
      show_rug <- FALSE
    } else {
      show_rug <- TRUE
    }
    if (show_dens_only) {
      par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 2) + 0.1)
    } else {
      par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 2) + 0.1)
    }
    # Set the range of values for the x-axis of the bottom plot
    for_qfun <- c(list(p = bottom_p_vec ^ (1 / n)), fun_args)
    bottom_range <- do.call(qfun, for_qfun)
    # Do the simulation (if required)
    if (!show_dens_only) {
      sim_list <- c(list(n = n), fun_args)
      if (old_pdf_or_cdf == pdf_or_cdf & old_show_dens == show_dens &
          old_show_dens_only == show_dens_only) {
        temp <- as.matrix(replicate(n_add, do.call(rfun, sim_list)))
        max_y <- apply(temp, 2, max)
        # Extract the last dataset and the last maximum (for drawing the arrow)
        y <- temp[, n_add]
        assign("old_y", y, envir = envir)
        rm(temp)
        last_y <- max_y[n_add]
        assign("save_last_y", last_y, envir = envir)
      } else {
        max_y <- NULL
        y <- old_y
        last_y <- save_last_y
      }
      if (n != old_n) {
        sample_maxima <- max_y
      } else {
        sample_maxima <- c(sample_maxima, max_y)
      }
      assign("sample_maxima", sample_maxima, envir = envir)
    }
    #
    n_x_axis <- 501
    # Top plot --------
    #
    # Set range for x-axis
    x <- seq(top_range[1], top_range[2], len = n_x_axis)
    # Calculate the density over this range
    dens_list <- c(list(x = x), fun_args)
    ydens <- do.call(dfun, dens_list)
    # Remove any infinite values
    finite_vals <- is.finite(ydens)
    ydens <- ydens[finite_vals]
    x <- x[finite_vals]
    # Set the top of the y-axis
    ytop <- max(ydens) * 1.2
    # Extract the distribution name and parameters
    the_distn <-
      switch(distn,
        "exponential" = paste(distn, "(", fun_args$rate, ")"),
        "uniform" = paste(distn, "(", fun_args$min, ",", fun_args$max, ")"),
        "gp" = paste("GP", "(", fun_args$loc, ",", fun_args$scale, ",",
                     fun_args$shape, ")"),
        "normal" = paste(distn, "(", fun_args$mean, ",", fun_args$sd, ")"),
        "beta" = paste(distn, "(", fun_args$shape1, ",", fun_args$shape2, ")"),
        "t" = paste("Student t", "(", fun_args$df, ")"),
        "gamma" = paste(distn, "(", fun_args$shape, ",", fun_args$rate, ")"),
        "lognormal" = paste(distn, "(", fun_args$meanlog, ",", fun_args$sdlog,
                            ")"),
        "cauchy" = paste("Cauchy", "(", fun_args$location, ",", fun_args$scale,
                         ")"),
        "chi-squared" = paste(distn, "(", fun_args$df, ",", fun_args$ncp, ")"),
        "f" = paste("F", "(", fun_args$df1, ",", fun_args$df2, ",",
                    fun_args$ncp, ")"),
        "weibull" = paste("Weibull", "(", fun_args$shape, ",", fun_args$scale,
                          ")"),
        "ngev" = paste("negated GEV", "(", fun_args$loc, ",", fun_args$scale,
                       ",", fun_args$shape, ")")
      )
    if (!show_dens_only) {
      my_xlim <- pretty(c(y, top_range))
      my_xlim <- my_xlim[c(1, length(my_xlim))]
      # Histogram with rug
      graphics::hist(y, col = 8, probability = TRUE, axes = FALSE,
                     xlab = xlab, ylab = "density", main = "",
                     xlim = my_xlim, ylim = c(0, ytop))
      graphics::lines(x, ydens, xpd = TRUE, lwd = 2, lty = 2)
      graphics::axis(2)
      graphics::axis(1, line = 0.5)
      graphics::title(main = paste(the_distn, ",  n = ", n))
      graphics::legend(top_leg_pos, legend = expression(f(x)),
                       col = 1, lwd = 2, lty = 2, box.lty = 0)
      u_t <- par("usr")
      if (arrow) {
        graphics::segments(last_y, u_t[3], last_y, -10, col = "red", xpd = TRUE,
                           lwd = 2, lty = 2)
      }
      if (show_rug) {
        graphics::rug(y, line = 0.5, ticksize = 0.05)
      }
      graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
      u_t <- my_xlim
    }
    #
    # Bottom plot --------
    #
    my_xlab <- paste("sample maximum of", n, "values")
    bn <- do.call(qfun, c(list(p = 1 - 1 / n), fun_args))
    an <- (1 / n) / do.call(dfun, c(list(x = bn), fun_args))
    # Set the limiting GEV parameters
    gev_pars <-
      switch(distn,
             "exponential" = list(loc = bn, scale = an, shape = 0),
             "uniform" = list(loc = bn, scale = an, shape = -1),
             "gp" = list(loc = bn, scale = an, shape = fun_args$shape),
             "normal" = list(loc = bn, scale = an, shape = 0),
             "beta" = list(loc = bn, scale = an, shape = -1 / fun_args$shape2),
             "t" = list(loc = bn, scale = an, shape = 1 / fun_args$df),
             "gamma" = list(loc = bn, scale = an, shape = 0),
             "lognormal" = list(loc = bn, scale = an, shape = 0),
             "cauchy" = list(loc = bn, scale = an, shape = 1),
             "chi-squared" = list(loc = bn, scale = an, shape = 0),
             "f" = list(loc = bn, scale = an, shape = 2 / fun_args$df2),
             "weibull" = list(loc = bn, scale = an, shape = 0),
             "ngev" = list(loc = bn, scale = an, shape = 0)
      )
    for_qgev <- c(list(p = bottom_p_vec), gev_pars)
    gev_bottom_range <- do.call(revdbayes::qgev, for_qgev)
    if (!show_dens_only) {
      bottom_range <- range(c(bottom_range, gev_bottom_range), sample_maxima)
    } else {
      bottom_range <- range(c(bottom_range, gev_bottom_range))
    }
    # Set range for x-axis
    x <- seq(bottom_range[1], bottom_range[2], len = n_x_axis)
    # Calcuate the density over this range
    if (pdf_or_cdf == "pdf") {
      dens_list <- c(list(x = x), gev_pars)
      ygev <- do.call(revdbayes::dgev, dens_list)
    } else {
      dens_list <- c(list(q = x), gev_pars)
      ygev <- do.call(revdbayes::pgev, dens_list)
    }
    p_list <- c(list(q = x), fun_args, list(log.p = TRUE))
    d_list <- c(list(x = x), fun_args)
    if (pdf_or_cdf == "pdf") {
      temp <- exp((n - 1) * do.call(pfun, p_list))
      ytrue <- n * temp * do.call(dfun, d_list)
      my_ylab <- "pdf"
      # Set the top of the y-axis
      ytop <- max(ygev, ytrue) * 1.5
    } else{
      ytrue <- exp(n * do.call(pfun, p_list))
      my_ylab <- "cdf"
      # Set the top of the y-axis
      ytop <- 1
    }
    # Histogram with rug
    y <- sample_maxima
    if (length(sample_maxima) > 1000) {
      show_bottom_rug <- FALSE
    } else {
      show_bottom_rug <- TRUE
    }
    if (!show_dens_only) {
      my_xlim <- pretty(c(y, bottom_range))
    } else {
      my_xlim <- pretty(bottom_range)
    }
    my_xlim <- my_xlim[c(1, length(my_xlim))]
    my_col <- 8
    if (!show_dens_only) {
      if (pdf_or_cdf == "pdf") {
        graphics::hist(y, col = my_col, probability = TRUE, las = 1,
                       axes = FALSE, xlab = my_xlab, ylab = my_ylab, main = "",
                       xpd = TRUE, xlim = my_xlim, ylim = c(0, ytop))
      } else {
        ecdfy <- stats::ecdf(y)
        graphics::plot(ecdfy, col = my_col, las = 1, main = "",
                       axes = FALSE, xlab = my_xlab, ylab = my_ylab,
                       xpd = TRUE, xlim = my_xlim, ylim = c(0, ytop),
                       col.01line = 0)
      }
      if (show_dens) {
        graphics::lines(x, ygev, xpd = TRUE, lwd = 2, lty = 2)
        graphics::lines(x, ytrue, xpd = TRUE, lwd = 2, lty = 2, col = "red")
      }
    } else {
      matplot(x, cbind(ygev, ytrue), col = c("black", "red"), lwd = 2, lty = 2,
              ylab = my_ylab, las = 1, xlab = my_xlab, xlim = my_xlim,
              ylim = c(0, ytop), axes = FALSE, type = "l")
    }
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    if (show_dens_only) {
      graphics::title(paste(the_distn, ",  n = ", n))
    }
    if (!show_dens_only) {
      if (show_bottom_rug) {
        graphics::rug(y, line = 0.5, ticksize = 0.05)
      }
      graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    }
    u_b <- my_xlim
    my_leg_2 <- paste("GEV (", signif(gev_pars$loc, 2), ",",
                      signif(gev_pars$scale, 2), ",",
                      signif(gev_pars$shape, 2), ")" )
    if (pdf_or_cdf == "pdf") {
      my_leg_true <- expression(n * F ^ {n-1} * f)
      if (show_dens) {
        graphics::legend(bottom_leg_pos, legend = c(my_leg_2, my_leg_true),
                         col = 1:2, lwd = 2, lty = 2, box.lty = 0)
      }
    } else {
      my_leg_true <- expression(F ^ n)
      if (!show_dens_only) {
        if (show_dens) {
          graphics::legend(bottom_leg_pos,
                       legend = c(my_leg_2, my_leg_true, "empirical cdf"),
                       col = c(1:2, 8), lwd = 2, lty = c(2, 2, -1),
                       pch = c(-1, -1, 16), box.lty = 0)
        } else {
          graphics::legend(bottom_leg_pos,
                           legend = c(my_leg_2, my_leg_true, "empirical cdf"),
                           col = c(0, 0, 8), lwd = 2, lty = c(2, 2, -1),
                           pch = c(-1, -1, 16), box.lty = 0,
                           text.col = c(0, 0, 1))
        }
      } else {
        graphics::legend(bottom_leg_pos,
                         legend = c(my_leg_2, my_leg_true),
                         col = 1:2, lwd = 2, lty = 2, box.lty = 0)
      }
    }
    if (!show_dens_only) {
      top_ratio <- (last_y - u_t[1]) / (u_t[2] - u_t[1])
      top_loc <- u_b[1] + (u_b[2] - u_b[1]) * top_ratio
      if (arrow) {
        graphics::segments(top_loc, ytop * 2, top_loc, ytop, col = "red",
                         xpd = TRUE, lwd = 2, lty = 2)
        graphics::arrows(top_loc, ytop, last_y, 0, col = "red", lwd = 2, lty = 2,
                         xpd = TRUE, code = 2)
      }
    }
    old_n <- n
    assign("old_n", old_n, envir = envir)
    assign("old_pdf_or_cdf", pdf_or_cdf, envir = envir)
    assign("old_show_dens", show_dens, envir = envir)
    assign("old_show_dens_only", show_dens_only, envir = envir)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
