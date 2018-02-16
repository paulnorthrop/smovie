# ================================ binom_pmf ==================================

#' Binomial p.m.f. movie
#'
#' A movie to illustrate how the probability mass function (p.m.f.) of a
#' binomial (n, p) random variable depends on n and p.
#'
#' @param starting_n A numeric scalar.  The value of n for the first graph.
#' @param starting_p A numeric scalar.  The value of p for the first graph.
#' @param panel_plot A logical parameter that determines whether the plot
#'   is placed inside the panel (\code{TRUE}) or in the standard graphics
#'   window (\code{FALSE}).  If the plot is to be placed inside the panel
#'   then the tkrplot library is required.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @param delta_n A numeric scalar.  The amount by which n is increased
#'   (or decreased) after one click of the + (or -) button in the parameter
#'   window.
#' @param delta_p A numeric scalar.  The amount by which p is increased
#'   (or decreased) after one click of the + (or -) button in the parameter
#'   window.
#' @param observed_value A non-negative integer.  If \code{observed_value} is
#'   supplied then the corresponding line in the plot of the p.m.f. is coloured
#'   in red.  If \code{observed_value} is not an integer then
#'   \code{round(observed_value)} is used.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rpanel]{rp.doublebutton}}, not including \code{panel},
#'   \code{variable}, \code{title}, \code{step}, \code{action}, \code{initval},
#'   \code{range}.
#' @details The probability mass function of a binomial random variable with
#'   parameters \eqn{n} (the number of Bernoulli trials performed) and
#'   \eqn{p} (the probabilities of success on a each trial) is plotted.
#'   The values of \eqn{n} and \eqn{p} can be changed by clicking on the
#'   relevant buttons.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' binom_pmf()
#'
#' # Increase n and see what happens
#' binom_pmf(delta_n = 10)
#'
#' # Highlight observed value of 26 successes in 44 trials
#' binom_pmf(starting_n = 44, starting_p = 0.1, delta_p = 0.1,
#'           observed_value = 26)
#' }
#' @export
binom_pmf <- function(starting_n = 1, starting_p = 1 /2, panel_plot = TRUE,
                      hscale = NA, vscale = hscale, delta_n = 1,
                      delta_p = 0.05, observed_value = NA, ...) {
  temp <- set_scales(hscale, vscale)
  hscale <- temp$hscale
  vscale <- temp$vscale
  if (!is.na(observed_value) && observed_value < 0) {
    stop("observed_value cannot be negative")
  }
  observed_value <- round(observed_value)
  binomial_panel <- rpanel::rp.control("binomial(n,p) probabilities",
                                       n = starting_n, prob = starting_p,
                                       observed_value = observed_value)
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
    rpanel::rp.tkrplot(binomial_panel, redraw_plot, plot_binomial_pmf,
                       pos = "right", hscale = hscale, vscale = vscale,
                       background = "white")
    action <- panel_redraw
  } else {
    action <- plot_binomial_pmf
  }
  #
  prob <- starting_p
  n <- starting_n
  plot_binomial_pmf(list(n = starting_n, prob = starting_p,
                         observed_value = observed_value))
  rpanel::rp.doublebutton(panel = binomial_panel, variable = prob,
                          title = "P(success), p:", step = delta_p,
                          action = action, initval = starting_p,
                          range = c(0, 1), ...)
  rpanel::rp.doublebutton(panel = binomial_panel, variable = n, step = delta_n,
                          title = "number of trials, n:",
                          action = action, initval = starting_n,
                          range = c(1, NA), ...)
  invisible()
}

plot_binomial_pmf <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    probs <- dbinom(0:n, n, prob)
    graphics::plot(c(0,n), c(0,1), type = "n", xlab = "y", ylab = "P(Y=y)",
                   axes = FALSE, ylim = c(0, max(probs)), xlim= c(0, n),
                   bty = "l", las = 1)
    if (n < 51){
      graphics::axis(1, at = 0:n, labels = 0:n, cex.axis = 0.7)
    }
    else{
      graphics::axis(1)
    }
    graphics::box(bty = "l")
    graphics::axis(2, las = 1)
    if (is.null(observed_value)) {
      col <- 1
    } else {
      col <- rep(1, n + 1)
      col[observed_value + 1] <- 2
    }
    graphics::segments(0:n, rep(0, n + 1), 0:n, probs, lwd = 3, col = col)
    ptext <- as.character(round(prob, 3))
    graphics::title(paste("binomial p.m.f.:  n =", n, "  p =", ptext))
    graphics::par(old_par)
  })
  return(panel)
}
