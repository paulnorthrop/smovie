# ================================== corr_sim =================================

#' Sampling distribution of the correlation coefficient movie
#'
#' A movie to illustrate how the sampling distribution of the (Pearson
#' product moment) sample correlation coefficient \eqn{r} depends on the
#' sample size \eqn{n} and on the true correlation \eqn{\rho}.
#'
#' @param n An integer scalar.  The initial value of the sample size.
#'   Must not be less than 2.
#' @param rho A numeric scalar.  The initial value of the true correlation
#'   \eqn{\rho}.  Must be in [-1, 1].
#' @param panel_plot A logical parameter that determines whether the plot
#'   is placed inside the panel (\code{TRUE}) or in the standard graphics
#'   window (\code{FALSE}).  If the plot is to be placed inside the panel
#'   then the tkrplot library is required.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @param delta_n An integer scalar.  The amount by which the value of the
#'   sample size is increased/decreased after one click of the +/- button.
#' @param delta_rho A numeric scalar.  The amount by which the value of
#'   rho is increased/decreased after one click of the +/- button.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @details Random samples of size \eqn{n} are simulated from a bivariate
#'   distribution with the property that the correlation between the two
#'   variables is equal to the value of chosen by the user.
#'   More specifically, the data are simulated from a
#'   \href{https://en.wikipedia.org/wiki/Multivariate_normal_distribution}{bivariate normal distribution}
#'   in which each of the variables has a mean of 0 and a variance of 1.
#'
#'   The movie contains two plots.  On the top is a scatter plot of the
#'   simulated sample, illustrating the stength of the association between
#'   the simulated values of the variables.
#'   A new sample is produced by clicking the + button next to
#'   "simulate another sample of size n:". [Ignore the - button.]
#'   For each simulated sample the sample correlation coefficient \eqn{r} is
#'   calculated and displayed in the title of the top plot.
#'   The values of these sample correlation coefficients are stored and
#'   are plotted in the histogram in the bottom plot.  As we accumulate
#'   a large number of values in this histogram the shape of the sampling
#'   distribution of \eqn{r} emerges.
#'
#'   The values of the sample size \eqn{n} or true correlation coefficient
#'   \eqn{\rho} can be changed using the respective +/- buttons.
#'   If one of these is changed then the histogram in the bottom plot is
#'   reset using the sample correlation coefficient of the first sample
#'   simulated using the new combination of \eqn{n} and \eqn{\rho}.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' corr_sim(n = 2)
#' corr_sim(n = 10, delta_n = 10)
#' }
#' @export
corr_sim <- function(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                     vscale = hscale, delta_n = 1, delta_rho = 0.1, pos = 1,
                     envir = as.environment(pos)) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  # Assign variables to an environment so that they can be accessed inside
  # corr_sim_movie_plot()
  if (n < 2) {
    stop("n must not be less than 2")
  }
  n <- round(n, 0)
  if (rho < -1 || rho > 1) {
    stop("rho must be in [-1, 1]")
  }
  delta_n <- round(delta_n)
  nseed <- nseed_init <- 47
  rho_init <- rho
  nsim <- nsim_init <- n
  rvals <- NULL
  #
  assign("nseed_old", nseed_init, envir = envir)
  assign("rho_old", rho_init, envir = envir)
  assign("nsim_old", nsim_init, envir = envir)
  assign("rvals", rvals, envir = envir)
  #
  corr_sim_panel <- rpanel::rp.control("correlation", nsim = nsim_init,
                                       rho = rho_init, nseed = nseed_init,
                                       add_true_pdf = FALSE, envir = envir)
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
    rpanel::rp.tkrplot(corr_sim_panel, redraw_plot, corr_sim_movie_plot,
                       pos = "right", hscale = hscale, vscale = vscale,
                       background = "white")
    action <- panel_redraw
  } else {
    action <- corr_sim_movie_plot
  }
  #
  # Create buttons for movie
  rpanel::rp.doublebutton(corr_sim_panel, nseed, 1, range=c(1, 100000000),
                          repeatinterval = 20, initval = nseed_init,
                          title = "simulate another sample of size n:",
                          action = action)
  rpanel::rp.doublebutton(corr_sim_panel, nsim, delta_n, range = c(2, 1000),
                          repeatinterval = 20, initval = n,
                          title = "sample size, n:",
                          action = action)
  rpanel::rp.doublebutton(corr_sim_panel, rho, delta_rho, range = c(-1, 1),
                          repeatinterval = 20, initval = rho_init,
                          title = "correlation, rho:", action = action)
  rpanel::rp.checkbox(panel = corr_sim_panel, add_true_pdf,
                      labels = "add the true pdf",
                      action = action)
  rpanel::rp.do(corr_sim_panel, action = action)
  return(invisible())
}

# Function to be called by corr_sim_movie().

corr_sim_movie_plot <- function(panel){
  with(panel, {

    old_par <- graphics::par(no.readonly = TRUE)
    set.seed(nseed)
    graphics::par(mfrow = c(1, 1), bty = "l", las = 1, oma = c(0, 0, 0, 0))
    vals <- matrix(stats::rnorm(2 * nsim), ncol = 2, nrow = nsim, byrow = TRUE)
    x1 <- vals[, 1]
    x2 <- vals[, 2]
    y1 <- rho * x1 + sqrt(1 - rho ^ 2) * x2
    sim_vals <- cbind(x1, y1)
    nf <- graphics::layout(matrix(c(2, 1), 2, 1, byrow = TRUE),
                           heights=c(3, 1), widths = c(3, 3), TRUE)
    if (nseed != nseed_old & rho == rho_old & nsim == nsim_old){
      histplot <- TRUE
    } else if (add_true_pdf) {
      histplot <- TRUE
    } else {
      histplot <- FALSE
      rvals <- NULL
    }
    rvals <- c(rvals, stats::cor(sim_vals)[1, 2])
    assign("rvals", rvals, envir = envir)
    graphics::par(mar = c(2, 3, 1, 1))
    bins <- 0.05 - 0.025 * abs(rho)
    br <- seq(from = -1, to = 1, length = 2 / bins)
    if (histplot) {
      graphics::hist(rvals, freq = FALSE, col = 8, breaks = br, axes = FALSE,
                     main = "")
      graphics::title(main = "sampling distribution of r", font.main = 1)
    } else {
      graphics::hist(rvals, freq = FALSE, col = 8, breaks = br,
                     xlim = c(-1, 1), main = "", axes = FALSE)
    }
    graphics::axis(1)
    #
    # Add the density function, if required
    #
    true_pdf <- function(rval, n, rho) {
      tc <- (n - 2) * gamma(n - 1) * (1 - rho ^ 2) ^ ((n - 1) / 2) *
        (1 - rval ^ 2) ^ ((n - 4) / 2)
      bc <- sqrt(2 * pi) * gamma(n - 1 / 2) * (1 - rho * rval) ^ (n - 3 / 2)
      temp <- gsl::hyperg_2F1(a = 1 / 2, b = 1 / 2, c = (2 * n - 1) / 2,
                              x = (rho * rval + 1) / 2)
      return(tc * temp / bc)
    }
    if (add_true_pdf) {
      r_vec <- seq(from = -1, to = 1, len = 101)
      true_pdf_vec <- true_pdf(rval = r_vec, n = nsim, rho = rho)
      graphics::lines(r_vec, true_pdf_vec)
    }
    assign("nseed_old", nseed, envir = envir)
    assign("rho_old", rho, envir = envir)
    assign("nsim_old", nsim, envir = envir)
    graphics::par(mar=c(3, 3, 1, 1))
    graphics::plot(sim_vals, pch = 16, xlab = "x", ylab = "y",
                   xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5))
    rhoval <- round(rho, 2)
    rval <- round(cor(sim_vals)[1, 2], 2)
    ttxt <- paste("rho =", rhoval,", r =", rval,",  n =", nsim)
    graphics::title(ttxt, font.main = 1)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
