# ================================== corr_sim =================================

#' Sampling distribution of the Pearson correlation coefficient movie
#'
#' A movie to illustrate how the sampling distribution of the Pearson
#' product moment sample correlation coefficient \eqn{r} depends on the
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
#'   normal distribution
#'   \href{https://en.wikipedia.org/wiki/Multivariate_normal_distribution}{bivariate normal distribution}
#'   in which each of the variables has a mean of 0 and a variance of 1 and
#'   the correlation \eqn{\rho} between the variables is chosen by the user.
#'
#'   The movie contains two plots.  On the top is a scatter plot of the
#'   simulated sample, illustrating the stength of the association between
#'   the simulated values of the variables.
#'   A new sample is produced by clicking "simulate another sample.
#'   For each simulated sample the sample (Pearson product moment)
#'   correlation coefficient \eqn{r} is calculated and displayed in the
#'   title of the top plot.
#'
#'   The values of the sample correlation coefficients are stored and are
#'   plotted in a histogram in the bottom plot.  A rug displays the individual
#'   values, with the most recent value coloured red. As we accumulate a large
#'   number of values in this histogram the shape of the sampling
#'   distribution of \eqn{r} emerges.  The exact p.d.f. of \eqn{r} is
#'   superimposed on this histogram, as is the value of \eqn{\rho}.
#'
#'   The bottom plot can be changed in two ways:
#'   (i) a radio button can be pressed to replace the histogram and pdf with
#'   a plot of the empirical c.d.f. and exact cdf;
#'   (ii) the variable can be changed from \eqn{\rho} to Fisher's
#'   z-transformation \eqn{F(\rho) = arctanh(\rho) = [ln(1+\rho) - ln(1-\rho)]/2}.
#'   For sufficiently large values of \eqn{n}, \eqn{F(\rho)} has approximately
#'   a normal distribution with mean \eqn{\rho} and standard deviation
#'   \eqn{1 / (n - 3)}.
#'
#'   The values of the sample size \eqn{n} or true correlation coefficient
#'   \eqn{\rho} can be changed using the respective +/- buttons.
#'   If one of these is changed then the bottom plot is
#'   reset using the sample correlation coefficient of the first sample
#'   simulated using the new combination of \eqn{n} and \eqn{\rho}.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' correlation(rho = 0.8)
#' correlation(n = 10)
#' }
#' @export
correlation <- function(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
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
  pdf_or_cdf <- "pdf"
  #
  assign("nseed_old", nseed_init, envir = envir)
  assign("rho_old", rho_init, envir = envir)
  assign("nsim_old", nsim_init, envir = envir)
  assign("rvals", rvals, envir = envir)
  assign("fisher_z_old", fisher_z, envir = envir)
  assign("old_pdf_or_cdf", pdf_or_cdf, envir = envir)
  #
  corr_sim_panel <- rpanel::rp.control("correlation", nsim = nsim_init,
                                       rho = rho_init, nseed = nseed_init,
                                       fisher_z = FALSE, pdf_or_cdf = "pdf",
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
  rpanel::rp.doublebutton(corr_sim_panel, nsim, delta_n, range = c(6, 1000),
                          repeatinterval = 20, initval = n,
                          title = "sample size, n",
                          action = action, ...)
  rpanel::rp.doublebutton(corr_sim_panel, rho, delta_rho, range = c(-1, 1),
                          repeatinterval = 20, initval = rho_init,
                          title = "correlation, rho", action = action, ...)
  rpanel::rp.checkbox(panel = corr_sim_panel, fisher_z,
                      labels = "Fisher z-transformation",
                      action = action)
  rpanel::rp.radiogroup(panel= corr_sim_panel, pdf_or_cdf, c("pdf", "cdf"),
                        title = "pdf or cdf in bottom plot",
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
    # Colour for the histograms and empirical cdfs
    my_col <- 8
    cond1 <- (fisher_z_old & fisher_z) | (!fisher_z_old & !fisher_z)
    cond2 <- (rho == rho_old & nsim == nsim_old) | nsim != nsim_old
    cond3 <- (old_pdf_or_cdf == pdf_or_cdf)
    if (cond1 & cond2 & cond3){
      vals <- matrix(stats::rnorm(2 * nsim), ncol = 2, nrow = nsim,
                     byrow = TRUE)
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
    graphics::par(mar = c(4, 4.2, 1, 1))
    atanh_rho <- atanh(rho)
    # Calculate the true density (under sampling from a BV normal)
    if (!fisher_z | (fisher_z & nsim < 3)) {
      if (nsim <= 3) {
        ep <- 1e-2
        # Use the normal approximation to avoid numerical issues
        z_range <- stats::qnorm(p = c(0.01, 0.99), mean = atanh_rho,
                                sd = 1 / sqrt(4 - 3))
        r_range <- tanh(z_range)
      } else {
        # Use the normal approximation to avoid numerical issues
        z_range <- stats::qnorm(p = c(0.01, 0.99), mean = atanh_rho,
                                sd = 1 / sqrt(nsim - 3))
        r_range <- tanh(z_range)
      }
      r_range <- range(r_range, rvals)
      r_vec <- seq(from = r_range[1], to = r_range[2], len = 101)
      if (abs(rho) < 1 & nsim > 2) {
        if (pdf_or_cdf == "pdf") {
          true_pdf_vec <- SuppDists::dPearson(x = r_vec, N = nsim, rho = rho)
          my_ylim <- c(0, max(true_pdf_vec) * 1.25)
        } else {
          true_cdf_vec <- SuppDists::pPearson(q = r_vec, N = nsim, rho = rho)
          my_ylim <- 0:1
        }
      } else {
        if (pdf_or_cdf == "pdf") {
          my_ylim <- NULL
        } else {
          my_ylim <- 0:1
        }
      }
      if (pdf_or_cdf == "pdf") {
        if (abs(rho) < 1) {
          graphics::hist(rvals, freq = FALSE, col = my_col, xlim = r_range,
                         main = "", axes = FALSE, ylim = my_ylim, xlab = "r",
                         ylab = "pdf", cex.lab = 1.5)
        } else {
          graphics::plot(1, type = "h", xlim = c(-1, 1), ylim = c(0, 1),
                         lwd = 2, xlab = "r", ylab = "pdf", cex.lab = 1.5,
                         axes = FALSE)
          graphics::segments(rho, 0, rho, 1, lty = 1, lwd = 2, col = "blue")
          leg_pos <- ifelse(rho > 0, "topleft", "topright")
          graphics::legend(leg_pos, legend = expression(rho), col = "blue", lty = 1,
                           lwd = 2, box.lty = 0, cex = 1.5)
        }
      } else {
        ecdf_rvals <- stats::ecdf(rvals)
        leg_pos <- "topleft"
        if (abs(rho) < 1) {
          graphics::plot(ecdf_rvals, col = my_col, las = 1, main = "",
                       axes = FALSE, xlab = "r", ylab = "cdf", xpd = TRUE,
                       xlim = r_range, ylim = my_ylim, col.01line = 0,
                       cex.lab = 1.5, cex = 1.5)
          graphics::legend(leg_pos,
                           legend = c("exact cdf", "empirical cdf"),
                           col = c("black", 8), lty = c(1, -1), lwd = 2,
                           pch = c(-1, 16), box.lty = 0, cex = 1.5)
        } else {
          graphics::plot(c(-1, 1, 1, 1.2), c(0, 0, 1, 1), xlim = c(-1, 1),
                         ylim = c(0, 1), type = "l", lwd = 2, axes = FALSE,
                         xlab = "r", ylab = "cdf", cex.lab = 1.5, cex = 1.5)
          graphics::legend(leg_pos, legend = "exact cdf", col = "black",
                           lty = 1, lwd = 2, box.lty = 0, cex = 1.5)
        }
      }
      if (!fisher_z | (fisher_z & abs(rho) < 1)) {
        graphics::axis(2)
        graphics::rug(rvals, line = 0.5, ticksize = 0.05)
        graphics::rug(new_rval, line = 0.5, ticksize = 0.05, col = "red",
                      lwd = 2)
      }
      # Add the true density function, but only if |rho| is not equal to 1
      if (abs(rho) < 1 & nsim > 2) {
        if (pdf_or_cdf == "pdf") {
          graphics::lines(r_vec, true_pdf_vec, lwd = 2, col = "black")
          leg_pos <- ifelse(rho > 0, "topleft", "topright")
          graphics::legend(leg_pos, legend = c("exact pdf", expression(rho)),
                           col = c("black", "blue"), lty = c(1, 1), lwd = 2,
                           box.lty = 0, cex = 1.5)
          rtop <- SuppDists::dPearson(x = rho, N = nsim, rho = rho)
          graphics::segments(rho, 0, rho, rtop, lty = 1, lwd = 2, col = "blue")
        } else {
          graphics::lines(r_vec, true_cdf_vec, lwd = 2, col = "black")
          leg_pos <- "topleft"
          graphics::legend(leg_pos,
                           legend = c("exact cdf", "empirical cdf"),
                           col = c("black", 8), lty = c(1, -1), lwd = 2,
                           pch = c(-1, 16), box.lty = 0, cex = 1.5)
        }
      }
      if (!fisher_z | (fisher_z & abs(rho) < 1)) {
        graphics::axis(1, line = 0.5)
        graphics::axis(1, line = 0.5, at = c(-1, 1))
      }
    } else if (nsim > 2) {
      zvals <- atanh(rvals)
      z_range <- range(zvals)
      if (abs(rho) < 1 & nsim > 3) {
        # Use the normal approximation to avoid numerical issues
        z_range_2 <- stats::qnorm(p = c(0.01, 0.99), mean = atanh_rho,
                                  sd = 1 / sqrt(nsim - 3))
        z_range <- range(z_range, z_range_2)
        z_vec <- seq(from = z_range[1], to = z_range[2], len = 101)
        if (pdf_or_cdf == "pdf") {
          true_pdf_vec <- dFPearson(x = z_vec, N = nsim, rho = rho)
          approx_pdf_vec <- stats::dnorm(x = z_vec, mean = atanh_rho,
                                         sd = 1 / sqrt(nsim - 3))
          my_ylim <- c(0, max(true_pdf_vec) * 1.25)
        } else {
          true_cdf_vec <- pFPearson(q = z_vec, N = nsim, rho = rho)
          approx_cdf_vec <- stats::pnorm(q = z_vec, mean = atanh_rho,
                                         sd = 1 / sqrt(nsim - 3))
          my_ylim <- c(0, 1)
        }
      } else {
        if (pdf_or_cdf == "pdf") {
          my_ylim <- NULL
        } else {
          my_ylim <- 0:1
        }
      }
      new_zval <- atanh(new_rval)
      if (pdf_or_cdf == "pdf") {
        if (abs(rho) < 1) {
          graphics::hist(zvals, freq = FALSE, col = 8, main = "", axes = FALSE,
                         ylim = my_ylim, xlab = "arctanh(r)", cex.lab = 1.5,
                         xlim = z_range, ylab = "pdf")
        } else {
          graphics::plot(1, type = "n", xlim = c(-1, 1), ylim = c(0, 1),
                         lwd = 2, xlab = "r", ylab = "pdf",
                         axes = FALSE, ann = FALSE)
          graphics::axis(2, col = 0, col.ticks = 0, col.axis = "white")
          graphics::axis(1, line = 0.5, col = 0, col.ticks = 0,
                         col.axis = "white")
          cex_val <- 2
          if (rho == 1) {
            text(0, 0.5, "Fisher z value is always +Inf when rho = +1",
                 cex = cex_val)
          } else {
            text(0, 0.5, "Fisher z value is always -Inf when rho = -1",
                 cex = cex_val)
          }
        }
      } else {
        ecdf_zvals <- stats::ecdf(zvals)
        if (abs(rho) < 1) {
          graphics::plot(ecdf_zvals, col = my_col, las = 1, main = "",
                       axes = FALSE, xlab = "arctanh(r)", ylab = "cdf",
                       xpd = TRUE, xlim = z_range, ylim = my_ylim,
                       col.01line = 0, cex.lab = 1.5, cex = 1.5)
        } else {
          graphics::plot(1, type = "n", xlim = c(-1, 1), ylim = c(0, 1),
                         lwd = 2, xlab = "r", ylab = "pdf",
                         axes = FALSE, ann = FALSE)
          graphics::axis(2, col = 0, col.ticks = 0, col.axis = "white")
          graphics::axis(1, line = 0.5, col = 0, col.ticks = 0,
                         col.axis = "white")
          cex_val <- 2
          if (rho == 1) {
            text(0, 0.5, "Fisher z value is always +Inf when rho = +1",
                 cex = cex_val)
          } else {
            text(0, 0.5, "Fisher z value is always -Inf when rho = -1",
                 cex = cex_val)
          }
        }
      }
      if (!fisher_z | (fisher_z & abs(rho) < 1)) {
        graphics::axis(2)
        graphics::rug(zvals, line = 0.5, ticksize = 0.05)
        graphics::rug(new_zval, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
      }
      # Add the true density function, but only if |rho| is not equal to 1
      if (abs(rho) < 1 & nsim > 3) {
        if (pdf_or_cdf == "pdf") {
          graphics::lines(z_vec, true_pdf_vec, lwd = 2, col = "black")
          graphics::lines(z_vec, approx_pdf_vec, lwd = 2, col = "red", lty = 2)
          leg_pos <- ifelse(rho > 0, "topleft", "topright")
          graphics::legend(leg_pos, legend = c("exact", "approximate",
                                               expression(arctanh(rho))),
                           col = c("black", "red", "blue"), lty = c(1, 2, 1),
                           lwd = 2, box.lty = 0, cex = 1.5)
          graphics::segments(atanh_rho, 0, atanh_rho, dFPearson(atanh_rho,
                                                                nsim, rho),
                             lty = 1, lwd = 2, col = "blue")
        } else {
          graphics::lines(z_vec, true_cdf_vec, lwd = 2, col = "black")
          graphics::lines(z_vec, approx_cdf_vec, lwd = 2, col = "red", lty = 2)
          leg_pos <- "topleft"
          graphics::legend(leg_pos,
                           legend = c("exact", "approximate", "empirical cdf"),
                           col = c("black", "red", 8), lty = c(1, 2, -1),
                           lwd = 2, pch = c(-1, -1, 16), box.lty = 0, cex = 1.5)
        }
      } else {
        graphics::abline(v = atanh_rho, lty = 2, lwd = 2, col = "blue")
      }
      if (!fisher_z | (fisher_z & abs(rho) < 1)) {
        graphics::axis(1, line = 0.5)
        graphics::axis(1, line = 0.5, at = c(-1e10, 1e10))
      }
    }
    assign("nseed_old", nseed, envir = envir)
    assign("rho_old", rho, envir = envir)
    assign("nsim_old", nsim, envir = envir)
    assign("fisher_z_old", fisher_z, envir = envir)
    assign("old_pdf_or_cdf", pdf_or_cdf, envir = envir)
    graphics::par(mar = c(3, 3, 2, 1))
    graphics::plot(sim_vals, pch = 16, xlab = "x", ylab = "y",
                   xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5))
    rhoval <- round(rho, 2)
    rprint <- sprintf('%.2f', abs(cor(sim_vals)[1, 2]))
    rval <- cor(sim_vals)[1, 2]
    if (rval > 0 ) {
      ttxt <- substitute(paste(rho == rhoval, " , ", r == +rprint, " , ",
                               n == nsim),
                         list(rhoval = rhoval, rprint = rprint, nsim = nsim))
    } else {
      ttxt <- substitute(paste(rho == rhoval, " , ", r == -rprint, " , ",
                               n == nsim),
                         list(rhoval = rhoval, rprint = rprint, nsim = nsim))
    }
    graphics::title(main = ttxt, cex.main = 1.5)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
