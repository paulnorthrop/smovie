# =================================== cltq ====================================

#' Central Limit Theorem (CLT) for sample quantiles
#'
#' A movie to illustrate the ideas of the sampling distribution of the
#' sample 100\eqn{p}\% quantile and the central limit theorem for sample
#' quantiles.
#'
#' @param n An integer scalar.  The size of the samples drawn from the
#'   distribution chosen using \code{distn}.
#' @param p A numeric scalar in (0, 1).  The value of \eqn{p}.
#' @param distn A character scalar specifying the (continuous) distribution
#'   from which observations are sampled.   Distributions \code{"beta"},
#'   \code{"chisq"}, \code{"chi-squared"},
#'   \code{"exponential"}, \code{"f"}, \code{"gamma"},
#'   \code{"gev"}, \code{"gp"}, \code{"lognormal"},
#'   \code{"log-normal"}, \code{"normal"},
#'   \code{"t"}, \code{"uniform"} and \code{"weibull"} are
#'   recognised, case being ignored.
#'
#'   If \code{distn} is not supplied then \code{distn = "exponential"}
#'   is used.
#'
#'   The \code{"gev"} and \code{"gp"} cases use the
#'   \code{\link[revdbayes]{gev}} and \code{\link[revdbayes]{gp}}
#'   distributional functions in the
#'   \code{\link[revdbayes]{revdbayes}} package.
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
#'   \code{"gev"} (\code{shape = 0.2}).
#'   \code{"gamma"} (\code{shape = 2},
#'   \code{"gp"} (\code{shape = 0.1}),
#'   \code{"t"} (\code{df = 4}) and
#'   \code{"weibull"} (\code{shape = 2}).
#' @param type An integer between 1 and 9. The value of the argument
#'   \code{type} to be passed to \code{\link[stats]{quantile}} to when
#'   calculating a sample quantile.
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
#' @param arrow A logical scalar.  Should an arrow be included to show the
#'   simulated sample quantile from the top plot being placed into the
#'   bottom plot?
#' @param leg_cex The argument \code{cex} to \code{\link[graphics]{legend}}.
#'   Allows the size of the legend to be controlled manually.
#' @param ... Additional arguments to the rpanel functions
#'   \code{\link[rpanel]{rp.button}} and
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details Loosely speaking, a consequence of the CLT for sample quantiles
#'   is that the 100\eqn{p}\% sample quantile of a \strong{large number} of
#'   identically distributed random variables, each with probability density
#'   function \eqn{f} and 100\eqn{p}\% quantile \eqn{\xi(p)}, has
#'   \strong{approximately} a normal distribution.  See, for example,
#'   \href{https://doi.org/10.1007/b98855}{Lehmann (1999)} for a precise
#'   statement and conditions.
#'
#'   This movie considers examples where this limiting result holds and
#'   illustrates graphically the closeness of the limiting approximation
#'   provided by the relevant normal limit to the true finite-\eqn{n}
#'   distribution.
#'
#'   Samples of size \code{n} are repeatedly simulated from the distribution
#'   chosen using \code{distn}.  These samples are summarized using a plot
#'   that appears at the top of the movie screen.  For each sample the
#'   100\eqn{p}\% sample quantile of these \code{n} values is calculated,
#'   stored and added to another plot, situated below the first plot.
#'   This plot is either a histogram or an empirical c.d.f., chosen using a
#'   radio button.
#'   A \code{\link[graphics]{rug}} is added to a histogram provided that it
#'   contains no more than 1000 points.
#'
#'   The p.d.f. of the original variables is added to the top plot.
#'
#'   Once it starts, four aspects of this movie are controlled by the user.
#'   \itemize{
#'     \item{}{There are buttons to increase (+) or decrease (-) the sample
#'       size, that is, the number of values for which a sample quantile is
#'       calculated.}
#'     \item{}{Each time the button labelled "simulate another \code{n_add}
#'       samples of size n" is clicked \code{n_add} new samples are simulated
#'       and their sample quantile are added to the bottom histogram.}
#'     \item{}{There is a button to switch the bottom plot from displaying
#'       a histogram of the simulated sample quantiles and the limiting normal
#'       p.d.f. to the empirical c.d.f. of the simulated data and the limiting
#'       normal c.d.f.}
#'     \item{}{There is a checkbox to add to the bottom plot the approximate
#'       (large \eqn{n}) normal p.d.f./c.d.f. implied by the CLT for sample
#'       quantiles: the mean is equal to \eqn{\xi(p)} and standard deviation is
#'       equal to \eqn{\sqrt p \sqrt q  / n f(\xi(p))}, where \eqn{q = 1-p}}.
#'   }
#' @return Nothing is returned, only the animation is produced.
#' @references Lehman, E. L. (1999) \emph{Elements of Large-Sample Theory},
#'   Springer-Verlag, London. \doi{10.1007/b98855}
#' @seealso \code{\link{movies}}: a user-friendly menu panel.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @seealso \code{\link{clt}}: Central Limit Theorem.
#' @examples
#' # Exponential data
#' cltq()
#'
#' # Uniform data
#' cltq(distn = "t", params = list(df = 2))
#' @export
cltq <- function(n = 20, p = 0.5, distn, params = list(), type = 7,
                 panel_plot = TRUE, hscale = NA, vscale = hscale, n_add = 1,
                 delta_n = 1, arrow = TRUE, leg_cex = 1.25, ...) {
  if (!tcltk::is.tclObj(tcltk::tclRequire("BWidget"))) {
    message("Package BWidget was not found.")
    message("Please see the smovie README file for information.")
    return()
  }
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  # To add another distribution
  # 1. misc.R: add code to set_fun_args(), set_top_range(), set_leg_pos()
  # 2. add lines to rfun, dfun, qfun, pfun, distn_den and distn_sd
  # 3. cltqmovie_plot(): add to the_distn
  if (p <= 0 || p >= 1) {
    stop("p must be in (0, 1)")
  }
  if (!is.wholenumber(n)) {
    stop("n must be an integer")
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
  xlab <- "x"
  #
  distn <- tolower(distn)
  if (distn == "log-normal") {
    distn <- "lognormal"
  }
  if (distn == "chisq") {
    distn <- "chi-squared"
  }
  # Check that revdbayes is installed, if it is needed
  if (distn %in% c("gp", "gev")) {
    if (!requireNamespace("revdbayes", quietly = TRUE)) {
      stop("the revdbayes package is needed. Please install it.",
           call. = FALSE)
    }
  }
  # Set the density, distribution, quantile and simulation functions.
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
           "chi-squared" = stats::rchisq,
           "f" = stats::rf,
           "weibull" = stats::rweibull,
           "gev" = revdbayes::rgev,
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
           "chi-squared" = stats::dchisq,
           "f" = stats::df,
           "weibull" = stats::dweibull,
           "gev" = revdbayes::dgev)
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
           "chi-squared" = stats::qchisq,
           "f" = stats::qf,
           "weibull" = stats::qweibull,
           "gev" = revdbayes::qgev)
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
           "chi-squared" = stats::pchisq,
           "f" = stats::pf,
           "weibull" = stats::pweibull,
           "gev" = revdbayes::pgev)
  #
  discrete_distn <- FALSE
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
  # Set a unique panel name to enable saving of objects to the correct panel
  now_time <- strsplit(substr(date(), 12, 19), ":")[[1]]
  now_time <- paste(now_time[1], now_time[2], now_time[3], sep = "")
  my_panelname <- paste("cltq_", now_time, sep = "")
  old_n <- 0
  # Store the mean and standard deviation of the underlying distribution
  if (distn == "beta") {
    alpha <- fun_args$shape1
    beta <- fun_args$shape2
  }
  if (distn == "f") {
    df1 <- fun_args$df1
    df2 <- fun_args$df2
    lam <- fun_args$ncp
    num <- (2 * (df1 + lam) ^ 2 + (df1 + 2 * lam) * (df2 - 2)) *
      (df2 / df1) ^ 2
    den <- (df2 - 2) ^ 2 * (df2 - 4)
  }
  if (distn == "t") {
    nu <- fun_args$df
    ncp <- fun_args$ncp
  }
  # distn_den is the value of the density at the 100p% quantile
  distn_den <- do.call(qfun, c(list(p = p), fun_args))
  distn_sd <- sqrt(p * (1 - p)) /
    do.call(dfun, c(list(x = distn_den), fun_args))
  # Create buttons for movie
  show_dens <- FALSE
  pdf_or_cdf <- "pdf"
  old_y <- NULL
  save_last_y <- NULL
  sample_qs <- NULL
  cltqpanel <- rpanel::rp.control("central limit theorem for sample quantiles",
                                  panelname = my_panelname, n = n,
                                  n_add = n_add,
                                  dfun = dfun, qfun = qfun, rfun = rfun,
                                  pfun = pfun, fun_args = fun_args,
                                  distn = distn, top_range = top_range,
                                  top_p_vec = top_p_vec,
                                  bottom_p_vec = bottom_p_vec,
                                  pdf_or_cdf = pdf_or_cdf,
                                  show_dens = show_dens,
                                  top_leg_pos = top_leg_pos,
                                  bottom_leg_pos = bottom_leg_pos,
                                  xlab = xlab, arrow = arrow,
                                  distn_den = distn_den, distn_sd = distn_sd,
                                  p = p,  type = type,
                                  discrete_distn = discrete_distn,
                                  old_n = old_n, old_pdf_or_cdf = pdf_or_cdf,
                                  old_show_dens = show_dens,
                                  old_y = old_y, save_last_y = save_last_y,
                                  sample_qs = sample_qs,
                                  leg_cex = leg_cex)
  #
  redraw_plot <- NULL
  panel_redraw <- function(panel) {
    rpanel::rp.tkrreplot(panel = panel, name = redraw_plot)
    # rp.tkrreplot() doesn't update the panel automatically, so do it manually
    # Get ...
    panel$sample_qs <- rpanel::rp.var.get(my_panelname, "sample_qs")
    panel$old_n <- rpanel::rp.var.get(my_panelname, "old_n")
    panel$old_pdf_or_cdf <- rpanel::rp.var.get(my_panelname, "old_pdf_or_cdf")
    panel$old_show_dens <- rpanel::rp.var.get(my_panelname, "old_show_dens")
    panel$old_y <- rpanel::rp.var.get(my_panelname, "old_y")
    panel$save_last_y <- rpanel::rp.var.get(my_panelname, "save_last_y")
    # Put ...
    rpanel::rp.control.put(my_panelname, panel)
    return(panel)
  }
  if (panel_plot & !requireNamespace("tkrplot", quietly = TRUE)) {
    warning("tkrplot is not available so panel_plot has been set to FALSE.")
    panel_plot <- FALSE
  }
  if (panel_plot) {
    rpanel::rp.tkrplot(panel = cltqpanel, name = redraw_plot,
                       plotfun = cltqmovie_plot, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
  } else {
    action <- cltqmovie_plot
  }
  #
  rpanel::rp.doublebutton(panel = cltqpanel, variable = n, step = delta_n,
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
      rpanel::rp.button(panel = cltqpanel, action = action, title = my_title,
                        repeatdelay = 100, repeatinterval = 100, ...)
  } else {
    rpanel::rp.button(panel = cltqpanel, action = action, title = my_title,
                      ...)
  }
  rpanel::rp.radiogroup(panel= cltqpanel, pdf_or_cdf, c("pdf", "cdf"),
                        title = "pdf or cdf in bottom plot", action = action)
  rpanel::rp.checkbox(panel = cltqpanel, show_dens,
                      labels = "show normal pdf/cdf", action = action)
  if (!panel_plot) {
    rpanel::rp.do(panel = cltqpanel, action = action)
  }
  return(invisible())
}

# Function to be called by cltq().

cltqmovie_plot <- function(panel) {
  oldpar <- graphics::par(mfrow = c(2, 1), oma = c(0, 0, 0, 0),
                          mar = c(4, 4, 2, 2) + 0.1)
  on.exit(graphics::par(oldpar))
  # To please R CMD check
  n <- distn <- fun_args <- pdf_or_cdf <- show_dens <- n_add <- rfun <-
    discrete_distn <- top_range <- dfun <- xlab <- top_leg_pos <- arrow <-
    distn_den <- distn_sd <- p <- type <- bottom_p_vec <- bottom_leg_pos <-
    leg_cex <- NULL
  panel <- within(panel, {
    # Don't add the rug in the top plot if n is large
    if (n > 1000) {
      show_rug <- FALSE
    } else {
      show_rug <- TRUE
    }
    # Do the simulation (if required)
    sim_list <- c(list(n = n), fun_args)
    if (old_pdf_or_cdf == pdf_or_cdf & old_show_dens == show_dens) {
      temp <- as.matrix(replicate(n_add, do.call(rfun, sim_list)))
      q_y <- apply(temp, 2, stats::quantile, probs = p, type = type)
      # Extract the last dataset and the last mean (for drawing the arrow)
      y <- temp[, n_add]
      old_y <- y
      rm(temp)
      last_y <- q_y[n_add]
      save_last_y <- last_y
    } else {
      q_y <- NULL
      y <- old_y
      last_y <- save_last_y
    }
    if (n != old_n) {
      sample_qs <- q_y
    } else {
      sample_qs <- c(sample_qs, q_y)
    }
    #
    n_x_axis <- 501
    # Top plot --------
    #
    # Set range for x-axis
    if (!discrete_distn) {
      x <- seq(top_range[1], top_range[2], len = n_x_axis)
    } else {
      x <- floor(top_range[1]):ceiling(top_range[2])
    }
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
        "t" = paste("Weibull", "(", fun_args$df, ",", fun_args$ncp, ")"),
        "gev" = paste("GEV", "(", fun_args$loc, ",", fun_args$scale,
                       ",", fun_args$shape, ")")
      )
    my_xlim <- pretty(c(y, top_range))
    my_xlim <- my_xlim[c(1, length(my_xlim))]
    if (!discrete_distn) {
      # Histogram with rug
      graphics::hist(y, col = 8, probability = TRUE, axes = FALSE,
                     xlab = xlab, ylab = "density", main = "",
                     xlim = my_xlim, ylim = c(0, ytop))
      graphics::lines(x, ydens, xpd = TRUE, lwd = 2, lty = 2)
      graphics::axis(2)
      graphics::axis(1, line = 0.5)
      graphics::title(main = paste(the_distn, ",  n = ", n))
      graphics::legend(top_leg_pos, legend = expression(f(x)),
                       col = 1, lwd = 2, lty = 2, box.lty = 0, cex = leg_cex)
      if (show_rug) {
        graphics::rug(y, line = 0.5, ticksize = 0.05)
      }
    } else {
      ep <- (my_xlim[2] - my_xlim[1] ) / 50
      my_xlim[2] <- my_xlim[2] + ep
      y_vals <- table(y)
      x_vals <- as.numeric(names(y_vals))
      graphics::matplot(x_vals, y_vals, type = "n", xlab = xlab,
                     ylab = "probability", axes = FALSE, xlim = my_xlim,
                     ylim = c(0, ytop))
      graphics::segments(x_vals, 0, x_vals, y_vals / n, lty = 1)
      graphics::segments(x + ep, 0, x + ep, ydens, lty = 2)
      graphics::axis(2)
      graphics::axis(1, line = 0.5)
      graphics::title(main = paste(the_distn, ",  n = ", n))
      graphics::legend(top_leg_pos, legend = c("sample", "P(X = x)"),
                       col = 1, lwd = 2, lty = 1:2, box.lty = 0, cex = leg_cex)
    }
    u_t <- graphics::par("usr")
    if (arrow) {
      graphics::segments(last_y, u_t[3], last_y, -10, col = "red", xpd = TRUE,
                         lwd = 2, lty = 2)
    }
    graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    u_t <- my_xlim
    #
    # Bottom plot --------
    #
    my_xlab <- paste0("sample ", 100 * p, "% quantile of ", n, " values")
    # Set the mean and variance of the sample quantile parameters
    normal_pars <- list(mean = distn_den, sd = distn_sd / sqrt(n))
    for_qnorm <- c(list(p = bottom_p_vec), normal_pars)
    normal_bottom_range <- do.call(stats::qnorm, for_qnorm)
    bottom_range <- range(normal_bottom_range, sample_qs)
    # Set range for x-axis
    x <- seq(bottom_range[1], bottom_range[2], len = n_x_axis)
    # Calcuate the density over this range
    if (pdf_or_cdf == "pdf") {
      dens_list <- c(list(x = x), normal_pars)
      ynorm <- do.call(stats::dnorm, dens_list)
    } else {
      dens_list <- c(list(q = x), normal_pars)
      ynorm <- do.call(stats::pnorm, dens_list)
    }
    d_list <- c(list(x = x), fun_args, list(n = n))
    if (pdf_or_cdf == "pdf") {
      my_ylab <- "pdf"
      # Set the top of the y-axis
      ytop <- max(ynorm) * 1.5
    } else{
      my_ylab <- "cdf"
      # Set the top of the y-axis
      ytop <- 1
    }
    # Histogram with rug
    y <- sample_qs
    if (length(sample_qs) > 1000) {
      show_bottom_rug <- FALSE
    } else {
      show_bottom_rug <- TRUE
    }
    my_xlim <- pretty(c(y, bottom_range))
    my_xlim <- my_xlim[c(1, length(my_xlim))]
    my_col <- 8
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
      graphics::lines(x, ynorm, xpd = TRUE, lwd = 2, lty = 2)
    }
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    # Force bottom axis to fill the plot
    graphics::axis(1, line = 0.5, at = c(-1e10, 1e10))
    if (show_bottom_rug) {
      graphics::rug(y, line = 0.5, ticksize = 0.05)
    }
    graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    u_b <- my_xlim
    my_leg_2 <- paste("N (", signif(distn_den, 2), ",",
                      signif(distn_sd ^ 2, 2), "/ n )" )
    if (pdf_or_cdf == "pdf") {
      if (show_dens) {
        graphics::legend(bottom_leg_pos, legend = my_leg_2, col = 1:2, lwd = 2,
                         lty = 2, box.lty = 0, cex = leg_cex)
      }
    } else {
      if (show_dens) {
        graphics::legend("topleft",
                       legend = c(my_leg_2, "empirical cdf"),
                       col = c(1, 8), lwd = 2, lty = c(2, -1),
                       pch = c(-1, 16), box.lty = 0, cex = leg_cex)
      } else {
        graphics::legend("topleft",
                         legend = c(my_leg_2, "empirical cdf"),
                         col = c(0, 8), lwd = 2, lty = c(2, -1),
                         pch = c(-1, 16), box.lty = 0, text.col = c(0, 1),
                         cex = leg_cex)
      }
    }
    top_ratio <- (last_y - u_t[1]) / (u_t[2] - u_t[1])
    top_loc <- u_b[1] + (u_b[2] - u_b[1]) * top_ratio
    if (arrow) {
      graphics::segments(top_loc, ytop * 2, top_loc, ytop, col = "red",
                         xpd = TRUE, lwd = 2, lty = 2)
      graphics::arrows(top_loc, ytop, last_y, 0, col = "red", lwd = 2, lty = 2,
                       xpd = TRUE, code = 2)
    }
    old_n <- n
    old_pdf_or_cdf <- pdf_or_cdf
    old_show_dens <- show_dens
  })
  return(panel)
}
