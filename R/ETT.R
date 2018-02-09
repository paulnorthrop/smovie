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
#'   \code{"uniform"} and \code{"gp"} are recognised,
#'   case being ignored.
#' @param params A named list of additional arguments to be passed to the
#'   density function associated with distribution \code{distn}.
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
ett <- function(n = 30, distn = c("exponential", "uniform", "gp"),
                params = list(), n_add = 1, delta_n = 1, xlab = "x", pos = 1,
                envir = as.environment(pos), ...) {
  p_vec <- c(0.001, 0.999)
  #
  distn <- match.arg(distn)
  distn <- tolower(distn)
  # Set the density and quantile functions and simulation function
  rfun <-
    switch(distn,
           "exponential" = stats::rexp,
           "uniform" = stats::runif,
           "gp" = revdbayes::rgp)
  dfun <-
    switch(distn,
           "exponential" = stats::dexp,
           "uniform" = stats::dunif,
           "gp" = revdbayes::dgp)
  qfun <-
    switch(distn,
           "exponential" = stats::qexp,
           "uniform" = stats::qunif,
           "gp" = revdbayes::qgp)
  # Get the names of the parameters
  par_names <- names(formals(dfun))
  to_remove <- which(is.element(par_names, c("x", "log")))
  par_names <- par_names[-to_remove]
  # Set any parameters of dfun and qfun specified in params
  params_names <- names(params)
  is_par_name <- is.element(params_names, par_names)
  fun_args <- params[is_par_name]
  # Set the (minimum) ranges for the plots
  for_qfun <- c(list(p = p_vec), fun_args)
  top_range <- do.call(qfun, for_qfun)
  for_qfun <- c(list(p = p_vec ^ (1 / n)), fun_args)
  bottom_range <- do.call(qfun, for_qfun)
  # Make adjustments for certain distributions
  if (distn == "exponential") {
    if (is.null(fun_args$rate)) {
      fun_args$rate <- 1
    }
    top_range[1] <- 0
  }
  if (distn == "uniform") {
    if (is.null(fun_args$min)) {
      fun_args$min <- 0
    }
    if (is.null(fun_args$max)) {
      fun_args$max <- 1
    }
    top_range[1] <- fun_args$min
    top_range[2] <- fun_args$max
  }
  if (distn == "gp") {
    if (is.null(fun_args$loc)) {
      fun_args$loc <- 0
    }
    if (is.null(fun_args$scale)) {
      fun_args$scale <- 1
    }
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 0.1
    }
    top_range[1] <- fun_args$loc
    if (fun_args$shape < 0) {
      top_range[2] <- fun_args$loc - fun_args$scale / fun_args$shape
    }
  }
  # Assign variables to an environment so that they can be accessed inside
  # clt_exponential_movie_plot()
  old_n <- 0
  assign("old_n", old_n, envir = envir)
  assign("xlab", xlab, envir = envir)
  # Create buttons for movie
  ett_panel <- rpanel::rp.control("sample size", n = n, n_add = n_add,
                                  dfun = dfun, qfun = qfun, rfun = rfun,
                                  fun_args = fun_args, distn = distn,
                                  top_range = top_range,
                                  bottom_range = bottom_range,
                                  envir = envir)
  rpanel::rp.doublebutton(panel = ett_panel, variable = n, step = delta_n,
                          title = "sample size, n",
                          action = ett_movie_plot, initval = n,
                          range = c(1, NA), ...)
  if (n_add == 1) {
    rpanel::rp.button(panel = ett_panel, action = ett_movie_plot,
                      title = paste("simulate another sample of size n"), ...)
  } else {
    rpanel::rp.button(panel = ett_panel, action = ett_movie_plot,
                      title = paste("simulate another", n_add,
                                    "samples of size n"), ...)
  }
  rpanel::rp.do(ett_panel, ett_movie_plot)
  return(invisible())
}

# Function to be called by ett().

ett_movie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 2) + 0.1)
    assign("xlab", xlab, envir = envir)
    sim_list <- c(list(n = n), fun_args)
    temp <- replicate(n_add, do.call(rfun, sim_list))
    max_y <- apply(temp, 2, max)
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
    # Top plot --------
    #
    # Set range for x-axis
    x <- seq(top_range[1], top_range[2], len = 101)
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
    graphics::axis(2, at = pretty(c(y, top_range[2])))
    graphics::axis(1, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05)
    graphics::title(paste("sample size, n = ",n))
    #
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
    if (distn == "gp") {
      bn <- do.call(qfun, c(list(p = 1 - 1 / n), fun_args))
      print("bn")
      print(bn)
      an <- (1 / n) / do.call(dfun, c(list(x = bn), fun_args))
      print(an)
    }
    gev_pars <-
      switch(distn,
             "exponential" = list(loc = log(n) / fun_args$rate,
                                  scale  = 1 / fun_args$rate,
                                  shape = 0),
             "uniform" = list(loc = fun_args$min +
                                (fun_args$max - fun_args$min) * (1 - 1 / n),
                              scale  = (fun_args$max - fun_args$min) / n,
                              shape = -1),
             "gp" = list(loc = bn, scale = an, shape = fun_args$shape)
             )
    # Set range for x-axis
    x <- seq(bottom_range[1], bottom_range[2], len = 101)
    # Calcuate the density over this range
    dens_list <- c(list(x = x), gev_pars)
    ydens <- do.call(revdbayes::dgev, dens_list)
    # Calculate the densities to be plotted in the histogram
    temp <- graphics::hist(y, plot = FALSE)
    # Set the top of the y-axis
    ytop <- max(ydens) * 1.5
    # Histogram with rug
    y <- sample_maxima
    my_xlim <- pretty(c(y, bottom_range))
    my_xlim <- my_xlim[c(1, length(my_xlim))]
    graphics::hist(y, col = 8, probability = TRUE, las = 1, axes = FALSE,
         xlab = my_xlab, ylab = "density", main = "",
         xpd = TRUE, xlim = my_xlim, ylim = c(0, ytop))
    graphics::lines(x, ydens, xpd = TRUE, lwd = 2, lty = 2)
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    graphics::rug(y, line = 0.5, ticksize = 0.05, col = "red")
    u_b <- my_xlim
    my_leg_2 <- paste("GEV(", round(gev_pars$loc, 2), ",",
                      round(gev_pars$scale, 2), ",",
                      round(gev_pars$shape, 2), ")" )
    graphics::legend("topleft", legend = my_leg_2, lwd = 2, lty = 2)
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
