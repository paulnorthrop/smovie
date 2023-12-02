# =============================== mean_vs_median ==============================

#' Sample mean vs sample median
#'
#' A movie to compare the sampling distributions of the sample mean
#' and sample median based on a random sample of size \eqn{n} from
#' either a standard normal distribution or a standard Student's \eqn{t}
#' distribution.  An interesting comparison is between the normal
#' and Student t with 2 degrees of freedom cases (see \strong{Examples}).
#'
#' @param n An integer scalar.  The size of the samples drawn from a
#'   standard normal distribution.
#' @param t_df A positive scalar.  The degrees of freedom \code{df} of
#'   a Student t distribution, as in \code{\link[stats]{TDist}}.
#'   If \code{t_df} is not supplied then data are simulated from a standard
#'   normal distribution.
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
#'   simulated sample maximum from the top plot being placed into the
#'   bottom plot?
#' @param leg_cex The argument \code{cex} to \code{\link[graphics]{legend}}.
#'   Allows the size of the legend to be controlled manually.
#' @param ... Additional arguments to the rpanel functions
#'   \code{\link[rpanel]{rp.button}} and
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details The movie is based on simulating repeatedly samples of size
#'   \code{n} from either a standard normal N(0,1) distribution or a standard
#'   Student t distribution.  The latter is selected by supplying the degrees
#'   of freedom of this distribution, using \code{t_df}.  The movie contains
#'   three plots.  The top plot contains a histogram of the most recently
#'   simulated dataset, with the relevant probability density function (p.d.f.)
#'   superimposed.  A \code{\link[graphics]{rug}} is added to a histogram
#'   provided that it contains no more than 1000 points.
#'
#'   Each time a sample is simulated the sample mean and sample median are
#'   calculated.  These values are indicated on the top plot using an
#'   arrow (if \code{arrow = TRUE}) or a vertical (rug) line on the horizontal
#'   axis (\code{arrow = FALSE}), coloured red for the sample mean and blue for
#'   the sample median.
#'   If \code{arrow = TRUE} then the arrows show the positionings of most
#'   recent mean and median in the two plots below.  If \code{arrow = FALSE}
#'   then the rug lines are replicated in these plots.
#'
#'   The plot in the middle contains a histogram of
#'   the sample means of \emph{all} the simulated samples.
#'   The plot on the bottom contains a histogram of
#'   the sample medians of \emph{all} the simulated samples.
#'   A \code{\link[graphics]{rug}} is added to these histograms
#'   provided that they contains no more than 1000 points.
#'
#'   Once it starts, three aspects of this movie are controlled by the user.
#'   \itemize{
#'     \item There are buttons to increase (+) or decrease (-) the sample
#'       size, that is, the number of values over which a maximum is
#'       calculated.
#'     \item Each time the button labelled "simulate another \code{n_add}
#'       samples of size n" is clicked \code{n_add} new samples are simulated
#'       and their sample mean are added to the bottom histogram.
#'     \item For the N(0,1) case only, there is a checkbox to add to the
#'       bottom plot the p.d.f.s of the distribution of the sample mean and
#'       the (approximate, large \code{n}) distribution of the sample median.
#'   }
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{movies}}: a user-friendly menu panel.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' # Sampling from a standard normal distribution
#' mean_vs_median()
#'
#' # Sampling from a standard t(2) distribution
#' mean_vs_median(t_df = 2)
#' @export
mean_vs_median <- function(n = 10, t_df = NULL, panel_plot = TRUE, hscale = NA,
                           vscale = hscale, n_add = 1, delta_n = 1,
                           arrow = TRUE, leg_cex = 1.75, ...) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  # Check values of n, n_add and delta_n
  if (!is.wholenumber(n)) {
    stop("n must be an integer")
  }
  if (!is.wholenumber(n_add) | n_add < 1) {
    stop("n_add must be an integer that is no smaller than 1")
  }
  if (!is.wholenumber(delta_n) | delta_n < 1) {
    stop("delta_n must be an integer that is no smaller than 1")
  }
  # Set a unique panel name to enable saving of objects to the correct panel
  now_time <- strsplit(substr(date(), 12, 19), ":")[[1]]
  now_time <- paste(now_time[1], now_time[2], now_time[3], sep = "")
  my_panelname <- paste("mean_vs_median_", now_time, sep = "")
  # Create buttons for movie
  show_dens <- FALSE
  old_n <- 0
  sample_means <- NULL
  sample_medians <- NULL
  save_last_mean <- NULL
  save_last_median <- NULL
  mean_vs_median_panel <- rpanel::rp.control("mean vs median",
                                 panelname = my_panelname, n = n,
                                 n_add = n_add, old_n = old_n, ntop = 1000,
                                 sample_means = sample_means,
                                 sample_medians = sample_medians,
                                 arrow = arrow, show_dens = show_dens,
                                 old_show_dens = show_dens, leg_cex = leg_cex,
                                 save_last_mean = save_last_mean,
                                 save_last_median = save_last_median,
                                 t_df = t_df)
  #
  redraw_plot <- NULL
  panel_redraw <- function(panel) {
    rpanel::rp.tkrreplot(panel = panel, name = redraw_plot)
    # rp.tkrreplot() doesn't update the panel automatically, so do it manually
    # Get ...
    panel$sample_means <- rpanel::rp.var.get(my_panelname, "sample_means")
    panel$sample_medians <- rpanel::rp.var.get(my_panelname, "sample_medians")
    panel$old_show_dens <- rpanel::rp.var.get(my_panelname, "old_show_dens")
    panel$old_y <- rpanel::rp.var.get(my_panelname, "old_y")
    panel$old_n <- rpanel::rp.var.get(my_panelname, "old_n")
    panel$save_last_mean <- rpanel::rp.var.get(my_panelname, "save_last_mean")
    panel$save_last_median <- rpanel::rp.var.get(my_panelname,
                                                 "save_last_median")
    # Put ...
    rpanel::rp.control.put(my_panelname, panel)
    return(panel)
  }
  if (panel_plot & !requireNamespace("tkrplot", quietly = TRUE)) {
    warning("tkrplot is not available so panel_plot has been set to FALSE.")
    panel_plot <- FALSE
  }
  if (panel_plot) {
    rpanel::rp.tkrplot(panel = mean_vs_median_panel, name = redraw_plot,
                       plotfun = mean_vs_median_plot, pos = "right",
                       hscale = hscale, vscale = vscale, background = "white")
    action <- panel_redraw
  } else {
    action <- mean_vs_median_plot
  }
  #
  rpanel::rp.doublebutton(panel = mean_vs_median_panel, variable = n,
                          step = delta_n, title = "sample size, n",
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
    rpanel::rp.button(panel = mean_vs_median_panel, action = action,
                      title = my_title, repeatdelay = 100,
                      repeatinterval = 100, ...)
  } else {
    rpanel::rp.button(panel = mean_vs_median_panel, action = action,
                      title = my_title, ...)
  }
  if (is.null(t_df)) {
    rpanel::rp.checkbox(panel = mean_vs_median_panel, show_dens,
                        labels = "show sampling pdfs", action = action)
  }
  if (!panel_plot) {
    rpanel::rp.do(panel = mean_vs_median_panel, action = action)
  }
  return(invisible())
}

# Function to be called by mean_vs_median().

mean_vs_median_plot <- function(panel) {
  oldpar <- graphics::par(mfrow = c(3, 1), oma = c(0, 0, 0, 0),
                          mar = c(4, 4, 2, 2) + 0.1, cex.axis = 1.5,
                          cex.lab = 1.5)
  on.exit(graphics::par(oldpar))
  # To please R CMD check
  n <- n_add <- arrow <- leg_cex <- show_dens <- t_df <- x <- NULL
  panel <- within(panel, {
    # Don't add the rug in the top plot if n is large
    if (n > 1000) {
      show_rug <- FALSE
    } else {
      show_rug <- TRUE
    }
    mu <- 0
    sigma <- 1
    if (old_show_dens == show_dens) {
      if (is.null(t_df)) {
        temp <- as.matrix(replicate(n_add, stats::rnorm(n, mean = mu,
                                                        sd = sigma)))
      } else {
        temp <- as.matrix(replicate(n_add, stats::rt(n, df = t_df)))
      }
      mean_y <- apply(temp, 2, mean)
      median_y <- apply(temp, 2, stats::median)
      # Extract the last dataset and the last mean and median
      # (for drawing the arrow)
      y <- temp[, n_add]
      old_y <- y
      rm(temp)
      last_mean <- mean_y[n_add]
      last_median <- median_y[n_add]
      save_last_mean <- last_mean
      save_last_median <- last_median
    } else {
      mean_y <- NULL
      median_y <- NULL
      y <- old_y
      last_mean <- save_last_mean
      last_median <- save_last_median
    }
    #
    if (n != old_n) {
      sample_means <- mean_y
      sample_medians <- median_y
    } else {
      sample_means <- c(sample_means, mean_y)
      sample_medians <- c(sample_medians, median_y)
    }
    h.low <- -2.5
    h.up <- 2.5
    br <- seq(from = h.low, to = h.up, by = 0.5)
    ytop <- stats::dnorm(0, sd = sigma) * 2
    y <- y[y > h.low & y < h.up]
    br2 <- sort(c(seq(from = floor(h.low), to = ceiling(h.up), by = 1), mu))
    #
    ## histogram with rug
    my_xlim <- c(h.low, h.up)
    graphics::hist(y, col = 8, probability = TRUE,
                   axes = FALSE, xlab = "raw data", ylab = "density",
                   main = "", ylim = c(0, ytop), xlim = my_xlim)
    graphics::axis(2)
    graphics::axis(1, at = br2, labels = br2, line = 0.5)
    if (show_rug) {
      graphics::rug(y, line = 0.5, ticksize = 0.05)
    }
    graphics::legend("topleft", legend = paste("n = ", n), cex = leg_cex,
                     text.font = 2, box.lty = 0)
    xx <- seq(from = h.low, to = h.up, len = 500)
    ydens <- stats::dnorm(xx, mean = mu, sd = sigma)
    graphics::lines(xx, ydens, xpd = TRUE, lwd = 2, lty = 2)
    u <- graphics::par("usr")
    if (is.null(t_df)) {
      graphics::legend("topright", legend = expression(paste("N(0,",1,")")),
                       lty = 2, lwd = 3, box.lty = 0, cex = leg_cex)
    } else {
      graphics::legend("topright", legend = paste("t with", t_df, "df",
                                                  sep = " "),
                       lty = 2, lwd = 3, box.lty = 0, cex = leg_cex)
    }
    if (last_mean > h.low & last_mean < h.up) {
      graphics::rug(last_mean, line = 0.5, ticksize = 0.05, col = "red", lwd = 2)
    }
    if (last_median > h.low & last_median < h.up) {
      graphics::rug(last_median, line = 0.5, ticksize = 0.05, col = "blue",
                  lwd = 2)
    }
    if (arrow) {
      graphics::segments(last_mean, 0, last_mean, -10, col = "red", xpd = TRUE,
                       lwd = 2)
      graphics::segments(last_median, 0, last_median, -10, col = "blue",
                         xpd = TRUE, lwd = 2)
    }
    ytop <- stats::dnorm(0, sd = sigma / sqrt(n)) * 1.2
    if (n <= 25) {
      my.by <- 0.1
    }
    if (n > 25) {
      my.by <- 0.05
    }
    my.by.2 <- 1
    # Means
    y <- sample_means
    y <- y[y > h.low & y < h.up]
    br <- seq(from = h.low, to = h.up, by = my.by)
    br2 <- sort(c(seq(from = floor(h.low), to = ceiling(h.up), by = my.by.2),
                  mu))
    ### histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, las = 1, breaks = br,
                   axes = FALSE, xlab = "sample mean", ylab = "density",
                   main = "", ylim = c(0, ytop), xpd = TRUE)
    graphics::axis(2)
    graphics::axis(1, at = br2, labels = br2, line = 0.5)
    nsim <- length(y)
    if (arrow) {
      if (nsim < 1000) {
        graphics::rug(y, line = 0.5, ticksize = 0.05, col = "red")
      }
    } else {
      if (nsim < 1000) {
        graphics::rug(y, line = 0.5, ticksize = 0.05, col = "black")
      }
      if (last_mean > h.low & last_mean < h.up) {
        graphics::rug(last_mean, line = 0.5, ticksize = 0.05, col = "red",
                    lwd = 2)
      }
    }
    if (show_dens) {
      graphics::curve(stats::dnorm(x, mean = mu, sd = sigma / sqrt(n)),
                    from = h.low, to = h.up, n = 500, bty="l",
                    ylab = "density", las = 1, xpd = TRUE, lwd = 3,
                    add = TRUE, lty = 2, col = 2)
      graphics::curve(stats::dnorm(x, mean = mu,
                                   sd = sqrt(1.57) * sigma / sqrt(n)),
                      from = h.low, to = h.up, n = 500, bty = "l",
                      ylab = "density",las = 1, xpd = TRUE, lwd = 1.5,
                      add = TRUE, lty = 2, col = 4)
      graphics::legend("topright", legend = c("N(0,1/n)", "N(0,1.57/n)"),
                       lty = 2, lwd = c(3, 1.5), col = c(2, 4),
                       box.lty = 0, cex = leg_cex)
    }
    if (arrow) {
      graphics::arrows(last_mean, 2 * ytop, last_mean, 0, col = "red", lwd = 2,
                       xpd = TRUE)
      graphics::segments(last_median, 2 * ytop, last_median, -10, col = "blue",
                       xpd = TRUE, lwd = 2)
    }
    # Medians
    y <- sample_medians
    y <- y[y > h.low & y < h.up]
    br <- seq(from = h.low, to = h.up, by = my.by)
    br2 <- sort(c(seq(from = floor(h.low), to = ceiling(h.up), by = my.by.2),
                  mu))
    ### histogram with rug
    graphics::hist(y, col = 8, probability = TRUE, las = 1, breaks = br,
                   axes = FALSE, xlab = "sample median", ylab = "density",
                   main = "", ylim = c(0, ytop), xpd = TRUE)
    graphics::axis(2)
    graphics::axis(1, at = br2, labels = br2, line = 0.5)
    if (arrow) {
      if (nsim < 1000) {
        graphics::rug(y, line = 0.5, ticksize = 0.05, col = "blue")
      }
    } else {
      if (nsim < 1000) {
        graphics::rug(y, line = 0.5, ticksize = 0.05, col = "black")
      }
      if (last_mean > h.low & last_mean < h.up) {
        graphics::rug(last_median, line = 0.5, ticksize = 0.05, col = "blue",
                    lwd = 2)
      }
    }
    if (show_dens) {
      graphics::curve(stats::dnorm(x, mean = mu,
                                 sd = sqrt(1.57) * sigma / sqrt(n)),
                    from = h.low, to = h.up, n = 500, bty = "l",
                    ylab = "density", las = 1, xpd = TRUE, lwd = 3,
                    add = TRUE, lty = 2, col = 4)
      graphics::curve(stats::dnorm(x, mean = mu, sd = sigma/sqrt(n)),
                      from = h.low, to = h.up, n = 500, bty = "l",
                      ylab = "density", las = 1, xpd = TRUE, lwd = 1.5,
                      add = TRUE, lty = 2, col = 2)
      graphics::legend("topright", legend = c("N(0,1.57/n)", "N(0,1/n)"),
                       lty = 2, lwd = c(3, 1.5), xjust = 1, col = c(4, 2),
                       box.lty = 0, cex = leg_cex)
    }
    if (arrow) {
      graphics::arrows(last_median, 2 * ytop, last_median, 0 , col = "blue",
                       lwd = 2, xpd = TRUE)
    }
    old_n <- n
    old_show_dens <- show_dens
  })
  return(panel)
}
