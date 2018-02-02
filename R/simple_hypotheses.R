# ================================== shypo ====================================

#' Testing simple hypotheses movie
#'
#' A movie to illustrate statistical concepts involved in the testing
#' of one simple hypothesis against another.  The example used is a
#' random sample from a normal distribution whose variance is assumed
#' to be known.
#'
#' @param mu0 A numeric scalar.  The value of \eqn{\mu} under the null
#'   hypothesis H0 with which to start the movie.
#' @param sd A positive numeric scalar.  The (common) standard deviation of
#'   the normal distributions of the data under the two hypotheses.
#'   of \eqn{\mu} under the alternative hypothesis is greater than \code{mu0}.
#' @param eff A numeric scalar.  The \emph{effect size}. The amount by which
#'   the value of \eqn{\mu} under the alternative hypothesis is greater than
#'   thevalue \code{mu0} under the null hypothesis.
#'   That is, \code{mu1} = \code{eff} + \code{mu0}.
#'   \code{eff} must be non-negative.
#' @param n A positive integer scalar.  The sample size with which to start
#'   the movie.
#' @param a A numeric scalar.  The critical value of the test with which to
#'   start the movie. H0 is rejected if the sample mean is greater than
#'   \code{a}.
#' @param target_alpha A numeric scalar in (0,1).  The target value of the
#'   type I error to be achieved by setting \code{a} and/or \code{n}
#'   if the user asks for this using a radio button.
#' @param target_beta A numeric scalar in (0,1).  The target value of the
#'   type II error to be achieved by setting \code{a} and/or \code{n}
#'   if the user asks for this using a radio button.
#' @param delta_mu0,delta_eff,delta_a,delta_n Numeric scalars.  The
#'   respective amounts by which the values of \code{mu0, eff, a} and
#'   \code{n} are increased (or decreased) after one click of the + (or -)
#'   button in the parameter window.
#' @details The movie is based on two plots.
#'
#'   The top plot shows the (normal)
#'   probability density functions of the sample mean under the null
#'   hypothesis H0 (mean \code{mu0}) and the alternative hypothesis H1
#'   (mean \code{mu1}, where \code{mu1} > \code{mu0}), with the values
#'   of \code{mu0} and \code{mu1} indicated by vertical dashed lines.
#'   H0 is rejected if the sample mean exceeds the critical value \code{a},
#'   which is indicated by a vertical black line.
#'
#'   The bottom plot shows how the probabilities of making a type I or type II
#'   error depend on the value of \code{a}, by plotting these probabilities
#'   against \code{a}.
#'
#'   A parameter window enables the user to change the values of \code{n},
#'   \code{a}, \code{mu0} or \code{eff} = \code{mu1} - \code{mu0} by clicking
#'   the +/- buttons.
#'
#'   Radio buttons can be used either to:
#'   \itemize{
#'     \item{}{set \code{a} to achieve the target type I error probability
#'             \code{target_alpha}, based on the current value of \code{n};}
#'     \item{}{set \code{a} and \code{n} to achieve the respective target
#'             type I and type II error probabilities of \code{target_alpha}
#'             and \code{target_beta}, based on the current value of \code{n}.}
#'   }
#'
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @examples
#' \dontrun{
#' # Start with the plot just after slide 10 of the STAT3001 notes
#' # See the effect of increasing a
#' shypo(mu0 = 0, eff = 5, n = 1)
#'
#' # Repeat with a larger sample size
#' shypo(mu0 = 0, eff = 1, n = 30)
#'
#' # First example on slide 13 of STAT3001 notes
#' shypo(mu0 = 0, eff = 5, n = 16, delta_a = 0.01)
#'
#' # Second example on slide 13 of STAT3001 notes
#' shypo(mu0 = 0, eff = 5, n = 12.3, a = 2.75, delta_a = 0.01, delta_n = 0.01)
#' }
#' @export
shypo <- function(mu0 = 0, sd = 6, eff = sd, n = 30,
                                    a = mu0 + eff / 2,
                                    target_alpha = 0.05, target_beta = 0.1,
                                    delta_n = 1,
                                    delta_a = sd / (10 * sqrt(n)),
                                    delta_eff = sd, delta_mu0 = 1) {
  if (n < 1) {
    stop("n must be no smaller than 1")
  }
  if (eff < 0) {
    stop("eff cannot be negative")
  }
  if (sd <= 0) {
    stop("sd must be positive")
  }
  # Create buttons for movie
  set_values <- "no"
  sh_panel <- rpanel::rp.control("Change n, a, mu0 or eff (= mu1 - mu0)",
                                 n = n, a = a, mu0 = mu0, eff = eff,
                                 sd = sd, target_alpha = target_alpha,
                                 target_beta = target_beta, set_values = "no")
  rpanel::rp.doublebutton(sh_panel, n, delta_n, range = c(1, NA), initval = n,
                          title = "sample size, n", action = sh_plot)
  rpanel::rp.doublebutton(sh_panel, a, delta_a, range = c(NA, NA), initval = a,
                          title = "critical value, a", action = sh_plot)
  rpanel::rp.doublebutton(sh_panel, mu0, delta_mu0, range = c(NA, NA),
                          initval = mu0, title = "mu under H0, mu0",
                          action = sh_plot)
  rpanel::rp.doublebutton(sh_panel, eff, delta_eff, range = c(0, NA),
                          initval = eff,
                          title = "eff size, eff = mu1 - mu0",
                          action = sh_plot)
  rpanel::rp.radiogroup(sh_panel, set_values,
                        c("no", "set a to achieve target alpha",
                          "set a and n to achieve target alpha and beta"),
                        action = sh_plot, title = "Set a and/or n automatically?")
  rpanel::rp.do(sh_panel, sh_plot)
  return(invisible())
}

# Function to be called by clt_normal_movie().

sh_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    mu1 <- mu0 + eff
    # Set a and/or n automatically if requested
    if (set_values == "set a to achieve target alpha") {
      z_alpha <- stats::qnorm(target_alpha, mean = 0, sd = 1,
                              lower.tail = FALSE)
      a <- sd * z_alpha / sqrt(n)
      print(a)
    } else if (set_values == "set a and n to achieve target alpha and beta") {
      z_alpha <- stats::qnorm(target_alpha, mean = 0, sd = 1,
                              lower.tail = FALSE)
      z_beta <- stats::qnorm(target_beta, mean = 0, sd = 1,
                             lower.tail = FALSE)
      n <- sd ^ 2 * (z_alpha + z_beta) ^ 2 / eff ^ 2
      a <- sd * z_alpha / sqrt(n)
      n <- ceiling(n)
      print(n)
      print(a)
    }
    # Set the standard error for later use, based on the current value of n
    se <- sd / sqrt(n)
    par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(3, 3, 2, 2) + 0.1)
    ## Top plot -----
    mult <- 3.5
    xlim <- c(mu0 - mult * se, mu1 + mult * se)
    x <- c(seq(xlim[1], xlim[2], len = 200), a)
    x <- sort(x)
    y0 <- stats::dnorm(x, mean = mu0, sd = se)
    y1 <- stats::dnorm(x, mean = mu1, sd = se)
    y <- cbind(y0, y1)
    graphics::matplot(x, y, col = c("blue", "red"), lwd = 2, type = "l",
                      lty = 1, axes = FALSE, ann = FALSE)
    xx0 <- c(x[x >= a], a)
    yy0 <- c(y0[x >= a], 0)
    xx1 <- c(x[x <= a], a)
    yy1 <- c(y1[x <= a], 0)
    graphics::polygon(xx0, yy0, col = "blue", density = 10)
    graphics::polygon(xx1, yy1, col = "red", density = 10, angle = -45)
    graphics::title(xlab = expression(bar(x)), line = 1.5, cex = 2)
    graphics::title(ylab = "density", line = 2)
    axis(1, pos = 0, mgp = c(3, 0.75, 0))
    axis(2, mgp = c(3, 0.75, 0))
    u <- graphics::par("usr")
    graphics::text(a, (u[3] - u[4]) / 20, "a", xpd = TRUE)
    round_mu0 <- round(mu0, 1)
    round_mu1 <- round(mu1, 1)
    round_var <- round(sd ^ 2, 1)
    graphics::legend("topleft", lty = 1, lwd = 2, col = "blue", bty = "n",
                     legend = paste("N(", round_mu0, ", ", round_var,
                                    " / n )", sep = ""), cex = 1.25)
    graphics::legend("topright", lty = 1, lwd = 2, col = "red", bty = "n",
                     legend = paste("N(", round_mu1, ", ", round_var,
                                    " / n )", sep = ""), cex = 1.25)
    graphics::segments(mu0, 0, mu0, dnorm(mu0, mean = mu0, sd = se),
                       col = "blue", lwd = 2, lty = 2)
    graphics::segments(mu1, 0, mu1, dnorm(mu1, mean = mu1, sd = se),
                       col = "red", lwd = 2, lty = 2)
    graphics::segments(a, 0, a, dnorm(mu1, mean = mu1, sd = se),
                       col = "black", lwd = 2, lty = 1)
    graphics::title(main = paste("n =", round(n, 2), ", a =", round(a, 2)),
                    cex.main = 1.5, font.main = 1)
    ## Bottom plot -----
    # Vector of values of the critical value a
    a_vec <- seq(xlim[1], xlim[2], len = 200)
    # Type 1 error
    alpha <- stats::pnorm(a_vec, mean = mu0, sd = se, lower.tail = FALSE)
    beta <- stats::pnorm(a_vec, mean = mu1, sd = se, lower.tail = TRUE)
    y <- cbind(beta, alpha)
    graphics::matplot(a_vec, y, col = c("red", "blue"), lwd = 2,
                      type = "l", lty = 1, axes = FALSE, ann = FALSE)
    graphics::title(xlab = "critical value", line = 1)
    graphics::title(ylab = "probability of error", line = 2)
    axis(1, pos = 0, mgp = c(3, 0.75, 0))
    axis(2, mgp = c(3, 0.75, 0))
    u <- graphics::par("usr")
    graphics::text(a, (u[3] - u[4]) / 20, "a", xpd = TRUE)
    graphics::segments(a, 0, a, 1, col = "black", lwd = 2, lty = 1)
    alpha_val <- stats::pnorm(a, mean = mu0, sd = se, lower.tail = FALSE)
    alpha_val <- round(alpha_val, 3)
    beta_val <- stats::pnorm(a, mean = mu1, sd = se, lower.tail = TRUE)
    beta_val <- round(beta_val, 3)
    alpha_text <- substitute(alpha == alpha_val, list(alpha_val = alpha_val))
    ab_text <- substitute(paste(beta == beta_val, " , ", alpha == alpha_val),
                          list(alpha_val = alpha_val, beta_val = beta_val))
    graphics::title(main = ab_text, cex.main = 1.5)
    graphics::legend("right", legend = expression(paste("type I, ", alpha)),
                     lty = 1, lwd = 2, col = "blue", bty = "n", cex = 1.25)
    graphics::legend("left", legend = expression(paste("type II, ", beta)),
                     lty = 1, lwd = 2, col = "red", bty = "n", cex = 1.25)
    graphics::par(old_par)
  })
  return(invisible(panel))
}
