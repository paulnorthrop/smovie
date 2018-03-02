# remove n > 1 restriction?
# exact for normal
# check set_leg_pos

# =================================== clt =====================================

#' Central Limit Theorem (CLT)
#'
#' A movie to illustrate the ideas of the sampling distribution of a mean
#' and the central limit theorem.
#'
#' @param n An integer scalar.  The size of the samples drawn from the
#'   distribution chosen using \code{distn}.  \code{n} must be no smaller
#'   than 2.
#' @param distn A character scalar specifying the distribution from which
#'   observations are sampled.   Distributions \code{"beta"},
#'   \code{"binomial"}, \code{"chisq"}, \code{"chi-squared"},
#'   \code{"exponential"}, \code{"f"}, \code{"gamma"}, \code{"geometric"},
#'   \code{"gev"}, \code{"gp"}, \code{"hypergeometric"}, \code{"lognormal"},
#'   \code{"log-normal"}, \code{"negative binomial"}, \code{"normal"},
#'   \code{"poisson"}, \code{"t"}, \code{"uniform"} and \code{"weibull"} are
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
#'   If \code{distn = "negative binomial"} then the \code{(size, prob)}
#'   parameterisation is used.  If \code{mu} is supplied via \code{params}
#'   then \code{prob} is inferred from this (and \code{size}).
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
#'   \code{"poisson"} (\code{lambda = 5}) and
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
#'   \href{https://en.wikipedia.org/wiki/Central_limit_theorem}{Central Limit Theorem (CLT)}
#'   (CLT) is that the mean of a \strong{large number} of independent and
#'   identically distributed random variables, each with mean \eqn{\mu} and
#'   finite standard deviation \eqn{\sigma} has \strong{approximately} a
#'   normal distribution, even if these original variables are not normally
#'   distributed.
#'
#'   This movie considers examples where this limiting result holds and
#'   illustrates graphically the closeness of the limiting approximation
#'   provided by the relevant normal limit to the true finite-\eqn{n}
#'   distribution.
#'
#'   Samples of size \code{n} are repeatedly simulated from the distribution
#'   chosen using \code{distn}.  These samples are summarized using a histogram
#'   that appears at the top of the movie screen.  For each sample the mean
#'   of these \code{n} values is calculated, stored and added to another
#'   histogram plotted below the first histogram.
#'
#'   The probability density function (p.d.f.) of the original
#'   variables is superimposed on the top histogram.  On the bottom histogram
#'   is superimposed the exact p.d.f. of the sample mean and the
#'   approximate (large \code{n}) normal p.d.f. (with mean \eqn{\mu} and standard
#'   deviation \eqn{\sigma / \sqrt{n}}) implied by the CLT.
#'
#'   Once it starts, four aspects of this movie are controlled by the user.
#'   \itemize{
#'     \item{}{There are buttons to increase (+) or decrease (-) the sample
#'       size, that is, the number of values over which a maximum is
#'       calculated.}
#'     \item{}{Each time the button labelled "simulate another \code{n_add}
#'       samples of size n" is clicked \code{n_add} new samples are simulated
#'       and their sample mean are added to the bottom histogram.}
#'     \item{}{There is a button to switch the bottom plot from displaying
#'       a histogram of the simulated data, the exact p.d.f. and the
#'       limiting normal p.d.f. to the empirical c.d.f. of the simulated data,
#'       the exact c.d.f. and the limiting normal c.d.f.}
#'     \item{}{There is a box that can be used to display only the bottom
#'       plot.  This option is selected automatically if the sample size
#'       \eqn{n} exceeds 100000.}
#'   }
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' # Exponential data
#' clt()
#'
#' # Uniform data
#' clt(distn = "uniform")
#'
#' # Poisson data
#' clt(distn = "poisson")
#' }
#' @export
clt <- function(n = 20, distn, params = list(), panel_plot = TRUE, hscale = NA,
                vscale = hscale, n_add = 1, delta_n = 1, arrow = TRUE,
                pos = 1, envir = as.environment(pos), ...) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  # To add another distribution
  # 1. misc.R: add code to set_fun_args(), set_top_range(), set_leg_pos()
  # 2. add lines to rfun, dfun, qfun, pfun, distn_mean and distn_sd
  # 3. cltmovie_plot(): add to the_distn
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
  xlab <- "x"
  #
  distn <- tolower(distn)
  if (distn == "log-normal") {
    distn <- "lognormal"
  }
  if (distn == "chisq") {
    distn <- "chi-squared"
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
           "binomial" = stats::rbinom,
           "geometric" = stats::rgeom,
           "hypergeometric" = stats::rhyper,
           "negative binomial" = stats::rnbinom,
           "poisson" = stats::rpois,
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
           "binomial" = stats::dbinom,
           "geometric" = stats::dgeom,
           "hypergeometric" = stats::dhyper,
           "negative binomial" = stats::dnbinom,
           "poisson" = stats::dpois,
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
           "binomial" = stats::qbinom,
           "geometric" = stats::qgeom,
           "hypergeometric" = stats::qhyper,
           "negative binomial" = stats::qnbinom,
           "poisson" = stats::qpois,
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
           "binomial" = stats::pbinom,
           "geometric" = stats::pgeom,
           "hypergeometric" = stats::phyper,
           "negative binomial" = stats::pnbinom,
           "poisson" = stats::ppois,
           "gev" = revdbayes::pgev)
  #
  discrete_distn <- FALSE
  if (distn %in% c("binomial", "geometric", "hypergeometric",
                   "negative binomial", "poisson")) {
    discrete_distn <- TRUE
  }
  # Set the arguments to the distributional functions
  fun_args <- set_fun_args(distn, dfun, fun_args, params)
  # Check for finite variance
  if (distn == "f") {
    if (fun_args$df2 <= 4) {
      stop("df2 must be greater than 4 for a finite variance")
    }
  }
  if (distn == "t") {
    if (fun_args$df <= 2) {
      stop("df must be greater than 2 for a finite variance")
    }
  }
  if (distn == "gp" || distn == "gev") {
    if (fun_args$shape >= 1/2) {
      stop("shape must be less than 1/2 for a finite variance")
    }
  }
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
  # Store the mean and standard deviation of the underlying distribution
  if (distn == "hypergeometric") {
    hp <- fun_args$m / (fun_args$m + fun_args$n)
  }
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
  if (distn == "gev") {
    mu <- fun_args$loc
    sigma <- fun_args$scale
    xi <- fun_args$shape
    if (xi == 0) {
      gev_mean <- mu - digamma(1) * sigma
      gev_var <- sigma ^ 2 / 6
    } else {
      gev_mean <- mu + sigma * (gamma(1 - xi) - 1) / xi
      gev_var <- sigma ^ 2 * (gamma(1 - 2 * xi) - gamma(1 - xi) ^ 2) / xi ^ 2
    }
  }
  distn_mean <- switch(distn,
                       "binomial" = fun_args$size * fun_args$prob,
                       "geometric" = (1 - fun_args$prob) / fun_args$prob,
                       "hypergeometric" = fun_args$k * hp,
                       "negative binomial" = fun_args$size *
                         (1 - fun_args$prob) / fun_args$prob,
                       "poisson" = fun_args$lambda,
                       "beta" = alpha / (alpha + beta),
                       "chi-squared" = fun_args$df + fun_args$ncp,
                       "f" = df2 * (df1 + lam) / (df1 * (df2 - 2)),
                       "gamma" = fun_args$shape / fun_args$rate,
                       "gp" = fun_args$loc + fun_args$scale /
                         (1 - fun_args$shape),
                       "gev" = gev_mean,
                       "lognormal" = exp(fun_args$meanlog +
                                           fun_args$sdlog ^ 2 / 2),
                       "normal" = fun_args$mean,
                       "t" = ncp * sqrt(nu / 2) * gamma((nu - 1) / 2) /
                         gamma(nu / 2),
                       "uniform" = (fun_args$min + fun_args$max) / 2,
                       "weibull" = fun_args$scale *
                         gamma(1 + 1 / fun_args$shape),
                       "exponential" = 1 / fun_args$rate)
  distn_sd <- switch(distn,
                     "binomial" = sqrt(fun_args$size * fun_args$prob *
                       (1 -fun_args$prob)),
                     "geometric" = sqrt(1 - fun_args$prob) / fun_args$prob,
                     "hypergeometric" = sqrt(fun_args$k * hp * (1 - hp) *
                       (fun_args$m + fun_args$n - fun_args$k) /
                         (fun_args$m + fun_args$n - 1)),
                     "negative binomial" = sqrt(fun_args$size *
                       (1 - fun_args$prob)) / fun_args$prob,
                     "poisson" = sqrt(fun_args$lambda),
                     "beta" = sqrt(alpha * beta / (alpha + beta) ^ 2 /
                       (alpha + beta + 1)),
                     "chi-squared" = sqrt(2 * (fun_args$df
                                               + 2 * fun_args$ncp)),
                     "f" = sqrt(num / den),
                     "gamma" = sqrt(fun_args$shape / fun_args$rate ^ 2),
                     "gp" = sqrt(fun_args$scale ^ 2 /
                                   (1 - fun_args$shape) ^ 2 /
                                   (1 - 2 * fun_args$shape)),
                     "gev" = sqrt(gev_var),
                     "lognormal" = sqrt(exp(2 * fun_args$meanlog +
                                         fun_args$sdlog ^ 2) *
                       (exp(fun_args$sdlog ^ 2) - 1)),
                     "normal" = fun_args$sd,
                     "t" = sqrt(nu * (1 + ncp ^ 2) / (nu - 2) + ncp ^ 2 * nu *
                       (gamma((nu - 1) / 2) - gamma(nu / 2)) ^ 2 /2),
                     "uniform" = sqrt((fun_args$max - fun_args$min) ^ 2 / 12),
                     "weibull" = sqrt(fun_args$scale ^ 2 *
                       gamma(1 + 2 / fun_args$shape) -
                       gamma(1 + 1 / fun_args$shape) ^ 2),
                     "exponential" = 1 / fun_args$rate)
  # Create buttons for movie
  pdf_or_cdf <- "pdf"
  assign("old_pdf_or_cdf", pdf_or_cdf, envir = envir)
  cltpanel <- rpanel::rp.control("central limit theorem", n = n, n_add = n_add,
                                 dfun = dfun, qfun = qfun, rfun = rfun,
                                 pfun = pfun, fun_args = fun_args,
                                 distn = distn, top_range = top_range,
                                 top_p_vec = top_p_vec,
                                 bottom_p_vec = bottom_p_vec,
                                 pdf_or_cdf = "pdf",
                                 top_leg_pos = top_leg_pos,
                                 bottom_leg_pos = bottom_leg_pos,
                                 xlab = xlab, arrow = arrow,
                                 distn_mean = distn_mean, distn_sd = distn_sd,
                                 discrete_distn = discrete_distn,
                                 envir = envir)
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
    # one arrow and sample mean appears in the first plot
    my_seed <- round(stats::runif(1, 0, 1000))
    set.seed(my_seed)
    rpanel::rp.tkrplot(panel = cltpanel, name = redraw_plot,
                       plotfun = cltmovie_plot, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
    set.seed(my_seed)
  } else {
    action <- cltmovie_plot
  }
  #
  rpanel::rp.doublebutton(panel = cltpanel, variable = n, step = delta_n,
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
      rpanel::rp.button(panel = cltpanel, action = action, title = my_title,
                        repeatdelay = 100, repeatinterval = 100, ...)
  } else {
    rpanel::rp.button(panel = cltpanel, action = action, title = my_title,
                      ...)
  }
  rpanel::rp.radiogroup(panel= cltpanel, pdf_or_cdf, c("pdf", "cdf"),
                        title = "pdf or cdf in bottom plot",
                        action = action)
  rpanel::rp.do(panel = cltpanel, action = action)
  return(invisible())
}

# Function to be called by ett().

cltmovie_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    # Don't add the rug in the top plot if n is large
    if (n > 1000) {
      show_rug <- FALSE
    } else {
      show_rug <- TRUE
    }
    par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 2, 2) + 0.1)
    # Do the simulation (if required)
    if (distn == "hypergeometric") {
      sim_list <- c(list(nn = n), fun_args)
    } else {
      sim_list <- c(list(n = n), fun_args)
    }
    if (old_pdf_or_cdf == pdf_or_cdf) {
      temp <- as.matrix(replicate(n_add, do.call(rfun, sim_list)))
      mean_y <- apply(temp, 2, mean)
      # Extract the last dataset and the last mean (for drawing the arrow)
      y <- temp[, n_add]
      assign("old_y", y, envir = envir)
      rm(temp)
      last_y <- mean_y[n_add]
      assign("save_last_y", last_y, envir = envir)
    } else {
      mean_y <- NULL
      y <- old_y
      last_y <- save_last_y
    }
    if (n != old_n) {
      sample_means <- mean_y
    } else {
      sample_means <- c(sample_means, mean_y)
    }
    assign("sample_means", sample_means, envir = envir)
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
        "binomial" = paste(distn, "(", fun_args$size, ",", fun_args$prob, ")"),
        "geometric" = paste(distn, "(", fun_args$prob, ")"),
        "hypergeometric" = paste(distn, "(", fun_args$m, ",", fun_args$n,
                      ",", fun_args$k, ")"),
        "negative binomial" = paste(distn, "(", fun_args$size, ",",
                                    fun_args$prob, ")"),
        "poisson" = paste("Poisson", "(", fun_args$lambda, ")"),
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
                       col = 1, lwd = 2, lty = 2, box.lty = 0)
      if (show_rug) {
        graphics::rug(y, line = 0.5, ticksize = 0.05)
      }
    } else {
      ep <- 0.2
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
                       col = 1, lwd = 2, lty = 1:2, box.lty = 0)
    }
    u_t <- par("usr")
    if (arrow) {
      graphics::segments(last_y, u_t[3], last_y, -10, col = "red", xpd = TRUE,
                         lwd = 2, lty = 2)
    }
    graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    u_t <- my_xlim
    #
    # Bottom plot --------
    #
    my_xlab <- paste("sample mean of", n, "values")
    # Set the mean and variance of the sample means parameters
    normal_pars <- list(mean = distn_mean, sd = distn_sd / sqrt(n))
    for_qnorm <- c(list(p = bottom_p_vec), normal_pars)
    normal_bottom_range <- do.call(stats::qnorm, for_qnorm)
    bottom_range <- range(normal_bottom_range, sample_means)
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
    y <- sample_means
    if (length(sample_means) > 1000) {
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
    graphics::lines(x, ynorm, xpd = TRUE, lwd = 2, lty = 2)
    graphics::axis(2)
    graphics::axis(1, line = 0.5)
    # Force bottom axis to fill the plot
    graphics::axis(1, line = 0.5, at = c(-1e10, 1e10))
    if (show_bottom_rug) {
      graphics::rug(y, line = 0.5, ticksize = 0.05)
    }
    graphics::rug(last_y, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    u_b <- my_xlim
    my_leg_2 <- paste("N (", signif(distn_mean, 2), ",",
                      signif(distn_sd ^ 2, 2), "/ n )" )
    if (pdf_or_cdf == "pdf") {
      graphics::legend(bottom_leg_pos, legend = my_leg_2, col = 1:2, lwd = 2,
                       lty = 2, box.lty = 0)
    } else {
      graphics::legend(bottom_leg_pos,
                       legend = c(my_leg_2, "empirical cdf"),
                       col = c(1, 8), lwd = 2, lty = c(2, -1),
                       pch = c(-1, 16), box.lty = 0)
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
    assign("old_n", old_n, envir = envir)
    assign("old_pdf_or_cdf", pdf_or_cdf, envir = envir)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
