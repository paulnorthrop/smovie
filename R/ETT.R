# =================================== ett =====================================

#' Extremal Types Theorem
#'
#' A movie to illustrate the extremal types theorem, that is, convergence
#' of the distribution of maxima of random samples from certain distributions
#' to a member of the Generalized Extreme Value (GEV) family.
#'
#' @param n An integer scalar.  The size of the samples drawn from the
#'   distribution chosen using \code{distn}.
#' @param distn A character scalar specifying the distribution from which
#'   observations are sampled..   Distributions \code{"exponential"},
#'   \code{"uniform"}, \code{"gp"}, \code{"normal"} and \code{"beta"} are
#'   recognised, case being ignored.
#'   The \code{"gp"} case uses the distributional functions
#'   \code{\link[revdbayes]{gp}} in the
#'   \code{\link[revdbayes]{revdbayes-package}}.  The other cases
#'   use the distributional functions in the
#'   \code{\link[stats]{stats-package}}.
#' @param params A named list of additional arguments to be passed to the
#'   density function associated with distribution \code{distn}.
#' @param panel_plot A logical parameter that determines whether the plot
#'   is placed inside the panel (\code{TRUE}) or in the standard graphics
#'   window (\code{FALSE}).  If the plot is to be placed inside the panel
#'   then the tkrplot library is required.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @param n_add An integer scalar.  The number of simulated datasets to add at
#'   to each new frame of the movie.
#' @param delta_n A numeric scalar.  The amount by which n is increased
#'   (or decreased) after one click of the + (or -) button in the parameter
#'   window.
#' @param xlab A character scalar.  A name to use to label the horizontal
#'   axis of the plots.
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
#'   is that, in many situations, the maximum of a \strong{large number} of
#'   independent random variables has \strong{approximately} a GEV
#'   distribution.
#'
#'   Samples of size \code{n} are repeatedly simulated from the distribution
#'   chosen using \code{distn}.  These samples are summarized using a histogram
#'   that appears at the top of the movie screen.  For each sample the maximum
#'   of these \code{n} values is calculated, stored and added to another
#'   histogram plotted below the first histogram.
#'   The probability density function (p.d.f.) of the original
#'   variables is superimposed on the top histogram.  On the bottom histogram
#'   is superimposed the approximate (large \code{n}) GEV p.d.f. implied by
#'   the ETT.
#'
#'   Once it starts, two aspects of this movie are controlled by the user.
#'   Firstly, there are buttons to increase (+) or decrease (-) the sample
#'   size, that is, the number of values over which a mean is calculated.
#'   Then there is a button labelled
#'   "simulate another \code{n_add} samples of size n".
#'   Each time this button is clicked \code{n_add} new samples are simulated
#'   and their sample maxima are added to the bottom histogram.
#'
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' ett()
#' ett(repeatdelay = 100, repeatinterval = 100)
#' ett(distn = "uniform")
#' ett(distn = "gp", params = list(shape = 0.5))
#' }
#' @export
ett <- function(n = 20, distn = c("exponential", "uniform", "gp", "normal",
                                  "beta"),
                params = list(), panel_plot = TRUE, hscale = NA,
                vscale = hscale, n_add = 1, delta_n = 1, xlab = "x", pos = 1,
                envir = as.environment(pos), ...) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  #
  p_vec <- c(0.001, 0.999)
  #
  distn <- match.arg(distn)
  distn <- tolower(distn)
  # Set the density and quantile functions and simulation function
  rfun <-
    switch(distn,
           "exponential" = stats::rexp,
           "uniform" = stats::runif,
           "gp" = revdbayes::rgp,
           "normal" = stats::rnorm,
           "beta" = stats::rbeta)
  dfun <-
    switch(distn,
           "exponential" = stats::dexp,
           "uniform" = stats::dunif,
           "gp" = revdbayes::dgp,
           "normal" = stats::dnorm,
           "beta" = stats::dbeta)
  qfun <-
    switch(distn,
           "exponential" = stats::qexp,
           "uniform" = stats::qunif,
           "gp" = revdbayes::qgp,
           "normal" = stats::qnorm,
           "beta" = stats::qbeta)
  pfun <-
    switch(distn,
           "exponential" = stats::pexp,
           "uniform" = stats::punif,
           "gp" = revdbayes::pgp,
           "normal" = stats::pnorm,
           "beta" = stats::pbeta)
  # Set the arguments to the distributional functions
  fun_args <- set_fun_args(distn, dfun, fun_args, params)
  # Set the range for the top plot
  top_range <- set_top_range(distn, p_vec, fun_args, qfun)
  # Assign variables to an environment so that they can be accessed inside
  # clt_exponential_movie_plot()
  old_n <- 0
  assign("old_n", old_n, envir = envir)
  assign("xlab", xlab, envir = envir)
  # Create buttons for movie
  ett_panel <- rpanel::rp.control("sample size", n = n, n_add = n_add,
                                  dfun = dfun, qfun = qfun, rfun = rfun,
                                  pfun = pfun, fun_args = fun_args,
                                  distn = distn, top_range = top_range,
                                  p_vec = p_vec, envir = envir)
  #
  panel_redraw <- function(panel) {
    rpanel::rp.tkrreplot(panel, redraw_plot)
    return(panel)
  }
  if (panel_plot & !requireNamespace("tkrplot", quietly = TRUE)) {
    warning("tkrplot is not available so panel_plot has been set to FALSE.")
    panel_plot <- FALSE
  }
  if (panel_plot) {
    # Set a seed and then reset it before the panel is redrawn so that only
    # one arrow and sample maximum appears in the first plot
    my_seed <- round(runif(1, 0, 1000))
    set.seed(my_seed)
    rpanel::rp.tkrplot(ett_panel, redraw_plot, ett_movie_plot, pos = "right",
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
                          range = c(2, NA), ...)
  if (n_add == 1) {
    my_title <- paste("simulate another sample of size n")
  } else {
    my_title <- title = paste("simulate another", n_add, "samples of size n")
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
  rpanel::rp.do(ett_panel, action)
  return(invisible())
}

# Function to be called by ett().

ett_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 2) + 0.1)
    assign("xlab", xlab, envir = envir)
    sim_list <- c(list(n = n), fun_args)
    temp <- as.matrix(replicate(n_add, do.call(rfun, sim_list)))
    max_y <- apply(temp, 2, max)
    # Set the range of values for the x-axis of the bottom plot
    for_qfun <- c(list(p = p_vec ^ (1 / n)), fun_args)
    bottom_range <- do.call(qfun, for_qfun)
    # Extract the last dataset and the last maximum (for drawing the arrow)
    y <- temp[, n_add]
    last_y <- max_y[n_add]
    if (n != old_n) {
      sample_maxima <- max_y
    } else {
      sample_maxima <- c(sample_maxima, max_y)
    }
    assign("sample_maxima", sample_maxima, envir = envir)
    #
    n_x_axis <- 501
    # Top plot --------
    #
    # Set range for x-axis
    x <- seq(top_range[1], top_range[2], len = n_x_axis)
    # Calcuate the density over this range
    dens_list <- c(list(x = x), fun_args)
    ydens <- do.call(dfun, dens_list)
    # Calculate the densities to be plotted in the histogram
    temp <- graphics::hist(y, plot = FALSE)
    # Set the top of the y-axis
    ytop <- max(ydens) * 1.2
    #
    # Histogram with rug
    my_xlim <- pretty(c(y, top_range))
    my_xlim <- my_xlim[c(1, length(my_xlim))]
    graphics::hist(y, col = 8, probability = TRUE, axes = FALSE,
                   xlab = xlab, ylab = "density", main = "",
                   xlim = my_xlim, ylim = c(0, ytop))
    graphics::lines(x, ydens, xpd = TRUE, lwd = 2, lty = 2)
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05)
    the_distn <-
      switch(distn,
        "exponential" = paste(distn, "(", fun_args$rate, ")"),
        "uniform" = paste(distn, "(", fun_args$min, ",", fun_args$max, ")"),
        "gp" = paste(distn, "(", fun_args$loc, ",", fun_args$scale, ",",
                     fun_args$shape, ")"),
        "normal" = paste(distn, "(", fun_args$mean, ",", fun_args$sd, ")"),
        "beta" = paste(distn, "(", fun_args$shape1, ",", fun_args$shape2, ")")
      )
    graphics::title(paste(the_distn, ",  n = ", n))
    graphics::legend("topleft", legend = expression(f(x)),
                     col = 1, lwd = 2, lty = 2, box.lty = 0)
    u_t <- par("usr")
    graphics::segments(last_y, u_t[3], last_y, -10, col = "red", xpd = TRUE,
                       lwd = 2, lty = 2)
    u_t <- my_xlim
    graphics::rug(y, line = 0.5, ticksize = 0.05)
    graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    my_xlab <- paste("sample maxima of", xlab)
    #
    # Bottom plot --------
    #
    # Set the relevant GEV parameters
    bn <- do.call(qfun, c(list(p = 1 - 1 / n), fun_args))
    an <- (1 / n) / do.call(dfun, c(list(x = bn), fun_args))
    gev_pars <-
      switch(distn,
             "exponential" = list(loc = bn, scale = an, shape = 0),
             "uniform" = list(loc = bn, scale = an, shape = -1),
             "gp" = list(loc = bn, scale = an, shape = fun_args$shape),
             "normal" = list(loc = bn, scale = an, shape = 0),
             "beta" = list(loc = bn, scale = an, shape = -1 / fun_args$shape2)
      )
    # Set range for x-axis
    x <- seq(bottom_range[1], bottom_range[2], len = n_x_axis)
    # Calcuate the density over this range
    dens_list <- c(list(x = x), gev_pars)
    ygev <- do.call(revdbayes::dgev, dens_list)
    p_list <- c(list(q = x), fun_args)
    d_list <- c(list(x = x), fun_args)
    ytrue <- n * do.call(pfun, p_list) ^ (n - 1) * do.call(dfun, d_list)
    # Calculate the densities to be plotted in the histogram
    temp <- graphics::hist(y, plot = FALSE)
    # Set the top of the y-axis
    ytop <- max(ygev) * 1.5
    # Histogram with rug
    y <- sample_maxima
    my_xlim <- pretty(c(y, bottom_range))
    my_xlim <- my_xlim[c(1, length(my_xlim))]
    graphics::hist(y, col = 8, probability = TRUE, las = 1, axes = FALSE,
         xlab = my_xlab, ylab = "density", main = "",
         xpd = TRUE, xlim = my_xlim, ylim = c(0, ytop))
    graphics::lines(x, ygev, xpd = TRUE, lwd = 2, lty = 2)
    graphics::lines(x, ytrue, xpd = TRUE, lwd = 2, lty = 2, col = "red")
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05, col = "red")
    u_b <- my_xlim
    my_leg_2 <- paste("GEV(", round(gev_pars$loc, 2), ",",
                      round(gev_pars$scale, 2), ",",
                      round(gev_pars$shape, 2), ")" )
    my_leg_true <- expression(n * F ^ {n-1} * f)
    graphics::legend("topleft", legend = c(my_leg_2, my_leg_true),
                     col = 1:2, lwd = 2, lty = 2, box.lty = 0)
    top_ratio <- (last_y - u_t[1]) / (u_t[2] - u_t[1])
    top_loc <- u_b[1] + (u_b[2] - u_b[1]) * top_ratio
    graphics::segments(top_loc, ytop * 2, top_loc, ytop, col = "red",
                       xpd = TRUE, lwd = 2, lty = 2)
    graphics::arrows(top_loc, ytop, last_y, 0, col = "red", lwd = 2, lty = 2,
                     xpd = TRUE, code = 2)
    old_n <- n
    assign("old_n", old_n, envir = envir)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
