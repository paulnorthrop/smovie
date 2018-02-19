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
#' @param ... Additional arguments to the rpanel functions
#'   \code{\link[rpanel]{rp.button}} and
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
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
#'   "simulate another sample:". [Ignore the - button.]
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
                     envir = as.environment(pos), ...) {
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
  fisher_z <- FALSE
  #
  assign("nseed_old", nseed_init, envir = envir)
  assign("rho_old", rho_init, envir = envir)
  assign("nsim_old", nsim_init, envir = envir)
  assign("rvals", rvals, envir = envir)
  assign("fisher_z_old", fisher_z, envir = envir)
  #
  corr_sim_panel <- rpanel::rp.control("correlation", nsim = nsim_init,
                                       rho = rho_init, nseed = nseed_init,
                                       fisher_z = FALSE, envir = envir)
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
    rpanel::rp.tkrplot(panel = corr_sim_panel, name = redraw_plot,
                       plotfun = corr_sim_movie_plot, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
  } else {
    action <- corr_sim_movie_plot
  }
  #
  # Create buttons for movie
  dlist <- list(...)
  # If the user hasn't set either repeatdelay or repeatinterval then set them
  # to the default values in rp.doublebutton (100 milliseconds)
  if (is.null(dlist$repeatdelay) & is.null(dlist$repeatinterval)) {
    rpanel::rp.button(panel = corr_sim_panel, action = action,
                      title = "simulate another sample",
                      repeatdelay = 100, repeatinterval = 100, ...)
  } else {
    rpanel::rp.button(panel = corr_sim_panel, action = action,
                      title = "simulate another sample",
                      ...)
  }
  rpanel::rp.doublebutton(corr_sim_panel, nsim, delta_n, range = c(2, 1000),
                          repeatinterval = 20, initval = n,
                          title = "sample size, n",
                          action = action, ...)
  rpanel::rp.doublebutton(corr_sim_panel, rho, delta_rho, range = c(-1, 1),
                          repeatinterval = 20, initval = rho_init,
                          title = "correlation, rho", action = action, ...)
  rpanel::rp.checkbox(panel = corr_sim_panel, fisher_z,
                      labels = "Fisher z-transformation",
                      action = action)
  return(invisible())
}

# Function to be called by corr_sim_movie().

corr_sim_movie_plot <- function(panel){
  with(panel, {

    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(1, 1), bty = "l", las = 1, oma = c(0, 0, 0, 0))
    if (rho != rho_old | nsim != nsim_old) {
      rvals <- NULL
    }
    cond1 <- (fisher_z_old & fisher_z) | (!fisher_z_old & !fisher_z)
    cond2 <- (rho == rho_old & nsim == nsim_old) | nsim != nsim_old
    if (cond1 & cond2){
      vals <- matrix(stats::rnorm(2 * nsim), ncol = 2, nrow = nsim, byrow = TRUE)
      assign("vals", vals, envir = envir)
    }
    x1 <- vals[, 1]
    x2 <- vals[, 2]
    y1 <- rho * x1 + sqrt(1 - rho ^ 2) * x2
    sim_vals <- cbind(x1, y1)
    nf <- layout(mat = matrix(c(0, 2, 2, 0,
                                1, 1, 1, 1), nrow = 2, byrow = TRUE))
    if ((fisher_z_old & fisher_z) | (!fisher_z_old & !fisher_z)) {
      new_rval <- stats::cor(sim_vals)[1, 2]
      rvals <- c(rvals, new_rval)
    } else {
      new_rval <- rvals[length(rvals)]
    }
    assign("rvals", rvals, envir = envir)
    graphics::par(mar = c(4, 3, 1, 1))
    bins <- 0.05 - 0.025 * abs(rho)
    br <- seq(from = -1, to = 1, length = 2 / bins)
    # Calculate the true density (under sampling from a BV normal)
    if (!fisher_z | (fisher_z & nsim < 3)) {
      r_vec <- seq(from = -1, to = 1, len = 1001)
      if (abs(rho) < 1 & nsim > 2) {
        true_pdf_vec <- SuppDists::dPearson(x = r_vec, N = nsim, rho = rho)
        my_ylim = c(0, max(true_pdf_vec) * 1.25)
      } else {
        my_ylim = NULL
      }
      graphics::hist(rvals, freq = FALSE, col = 8, breaks = br,
                     xlim = c(-1, 1), main = "", axes = FALSE,
                     ylim = my_ylim, xlab = "r", cex.lab = 1.5)
      graphics::rug(rvals, line = 0.5, ticksize = 0.05)
      graphics::rug(new_rval, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
      # Add the true density function, but only if |rho| is not equal to 1
      if (abs(rho) < 1 & nsim > 2) {
        graphics::lines(r_vec, true_pdf_vec, lwd = 2, col = "black")
        leg_pos <- ifelse(rho > 0, "topleft", "topright")
        graphics::legend(leg_pos, legend = c("exact density", "true rho"),
                         col = c("black", "blue"), lty = c(1, 2), lwd = 2)
        rtop <- SuppDists::dPearson(rho, nsim, rho)
        graphics::segments(rho, 0, rho, rtop, lty = 2, lwd = 2, col = "blue")
      } else {
        graphics::abline(v = rho, lty = 2, lwd = 2, col = "blue")
      }
      #
      graphics::axis(1, line = 0.5)
    } else if (nsim > 2) {
      zvals <- atanh(rvals)
      z_range <- range(zvals)
      if (abs(rho) < 1 & nsim > 3) {
        z_range_2 <- qFPearson(p = c(0.01, 0.99), N = nsim, rho = rho)
        z_range <- range(z_range, z_range_2)
        z_vec <- seq(from = z_range[1], to = z_range[2], len = 101)
        true_pdf_vec <- dFPearson(x = z_vec, N = nsim, rho = rho)
        my_ylim = c(0, max(true_pdf_vec) * 1.25)
      } else {
        my_ylim = NULL
      }
      new_zval <- atanh(new_rval)
      graphics::hist(zvals, freq = FALSE, col = 8, main = "", axes = FALSE,
                     ylim = my_ylim, xlab = "", cex.lab = 1.5, xlim = z_range)
      graphics::rug(zvals, line = 0.5, ticksize = 0.05)
      graphics::rug(new_zval, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
      # Add the true density function, but only if |rho| is not equal to 1
      if (abs(rho) < 1 & nsim > 3) {
        graphics::lines(z_vec, true_pdf_vec, lwd = 2, col = "black")
        leg_pos <- ifelse(rho > 0, "topleft", "topright")
        graphics::legend(leg_pos, legend = c("exact density", "true rho"),
                         col = c("black", "blue"), lty = c(1, 2), lwd = 2)
        graphics::segments(rho, 0, atanh(rho), dFPearson(atanh(rho), nsim, rho),
                           lty = 2, lwd = 2, col = "blue")
      } else {
        graphics::abline(v = atanh(rho), lty = 2, lwd = 2, col = "blue")
      }
      graphics::axis(1, line = 0.5)
    }
    assign("nseed_old", nseed, envir = envir)
    assign("rho_old", rho, envir = envir)
    assign("nsim_old", nsim, envir = envir)
    assign("fisher_z_old", fisher_z, envir = envir)
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
