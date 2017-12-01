# =================================== wws =====================================

#' Wald, Wilks and Score test movie
#'
#' A movie to illustrate the nature of the Wald, Wilks and score
#' likelihood-based test statistics, for a model with a scalar unknown
#' parameter.  The user can change the value of the parameter under a simple
#' null hypothesis and observe the effect on the test statistics.
#'
#' @param loglik An R function, vectorised with respect to its first argument
#'   that returns the value of the log-likelihood (up to an additive constant).
#' @param theta_range A numeric vector of length 2.  The range of values of
#'   \eqn{\theta} over which to plot the log-likelihood.
#' @param theta0 A numeric scalar.  The value of \eqn{\theta} under the null
#'   hypothesis to use at the start of the movie.
#' @param delta_theta0 A numeric scalar.  The amount by which the value of
#'   \code{theta0} is increased (or decreased) after one click of the + (or -)
#'   button in the parameter window.
#' @param theta_mle A numeric scalar.  The user may use this to supply the
#'   value of the maximum likelihood estimate (MLE) of \eqn{theta}.
#'   Otherwise, \code{\link[stats]{optim}} is used to search for the MLE,
#'   using \code{theta0} as the initial value and \code{theta_range} as
#'   bounds within which to search.
#' @param alg_score A R function that returns score function, that is, the
#'   derivative of \code{loglik} with respect to \eqn{\theta}.
#' @param ... Additional arguments to be passed to \code{loglik}.
#' @details Add details.
#' @return Nothing is returned, only the animation is produced.
#' @examples
#' # Load package rpanel
#' # [Use install.packages("rpanel") if necessary]
#' library(rpanel)
#'
#' \dontrun{
#' # Example 6.2 in the STAT3001 notes
#' wws(theta0 = 0.5)
#'
#' # Log-likelihood for a binomial experiment (up to an additive constant)
#' bin_loglik <- function(p, n_success, n_failure) {
#'   return(n_success * log(p) + n_failure * log(1 - p))
#' }
#'
#' wws(bin_loglik, theta0 = 0.5, theta_range = c(0.1, 0.7),
#'     theta_mle = 7 / 20, n_success = 7, n_failure = 13)
#'
#' bin_alg_score <- function(p, n_success, n_failure) {
#'   return(n_success / p - n_failure / (1 - p))
#' }
#' bin_alg_obs_info <- function(p, n_success, n_failure) {
#'   return(n_success / p ^ 2 + n_failure / (1 - p) ^ 2)
#' }
#' wws(bin_loglik, theta0 = 0.5, theta_range = c(0.1, 0.7),
#'     theta_mle = 7 / 20, n_success = 7, n_failure = 13,
#'     alg_score = bin_alg_score, alg_obs_info = bin_alg_obs_info)
#' }
#' @export
wws <- function(loglik = NULL, theta_range = c(0.1, 0.7),
                theta0 = mean(theta_range),
                delta_theta0 = abs(diff(theta_range)) / 20,
                theta_mle = NULL, alg_score = NULL, alg_obs_info = NULL,
                ...) {
  if (is.null(loglik)) {
    # Use the default (binomial) example
    loglik <- function(p, n_success, n_failure) {
     return(n_success * log(p) + n_failure * log(1 - p))
    }
    alg_score <- function(p, n_success, n_failure) {
      return(n_success / p - n_failure / (1 - p))
    }
    alg_obs_info <- function(p, n_success, n_failure) {
      return(n_success / p ^ 2 + n_failure / (1 - p) ^ 2)
    }
    user_args <- list(...)
    if (is.null(user_args$n_success)) {
      user_args$n_success <- 7
    }
    if (is.null(user_args$n_failure)) {
      user_args$n_failure <- 13
    }
    theta_mle <- user_args$n_success /
      (user_args$n_success + user_args$n_failure)
  } else {
    user_args <- list(...)
  }
  theta_range <- sort(theta_range)
  if (is.null(theta_mle)) {
    # Find the MLE, avoiding the printing of warning messages
    lower <- theta_range[1]
    upper <- theta_range[2]
    obfn <- function(x, ...) {
      check <- -loglik(x, ...)
      # method = L-BFGS-B doesn't like Inf
      if (is.infinite(check)) {
        check <- 1e10
      }
      return(check)
    }
    temp <- suppressWarnings(stats::optim(theta0, obfn, lower = lower,
                                          upper = upper, ...))
    print(temp)
    for_optim <- c(list(par = theta0, fn = obfn, lower = lower,
                        upper = upper), user_args)
    temp <- do.call(optim, for_optim)
    print(temp)
    loglik_at_mle <- -temp$value
  } else {
    for_mle <- c(list(theta_mle), user_args)
    loglik_at_mle <- do.call(loglik, for_mle)
  }
  if (is.null(alg_obs_info)) {
    for_optimHess <- c(list(par = theta_mle, fn = loglik), user_args)
    obs_info_at_mle <- -do.call(stats::optimHess, for_optimHess)
  } else {
    for_alg_obs_info <- c(list(theta_mle), user_args)
    obs_info_at_mle <- do.call(alg_obs_info, for_alg_obs_info)
  }
  # Create buttons for movie
  wws_panel <- rpanel::rp.control("Change theta0",
                                 loglik = loglik, theta_range = theta_range,
                                 theta0 = theta0, user_args = user_args,
                                 test_stat = "none",
                                 perform_tests = "no", theta_mle = theta_mle,
                                 loglik_at_mle = loglik_at_mle,
                                 alg_score = alg_score,
                                 alg_obs_info = alg_obs_info,
                                 obs_info_at_mle = obs_info_at_mle)
  rpanel::rp.doublebutton(wws_panel, theta0, delta_theta0,
                          range = c(theta_range[1], theta_range[2]),
                          initval = theta0,
                          title = "null value theta0 or theta",
                          action = wws_plot)
  rpanel::rp.radiogroup(wws_panel, test_stat,
                        c("none", "Wald", "Wilks", "score", "all"),
                        action = wws_plot,
                        title = "Choose the type of test statistic")
  rpanel::rp.radiogroup(wws_panel, perform_tests,
                        c("no", "yes"),
                        action = wws_plot,
                        title = "Calculate approximate p-values?")
  rpanel::rp.do(wws_panel, wws_plot)
  return(invisible())
}

# Function to be called by clt_normal_movie().

wws_plot <- function(panel) {
  with(panel, {
    old_par <- graphics::par(no.readonly = TRUE)
    par(oma = c(0, 0, 0, 0), mar = c(5, 5, 2, 4) + 0.1)
    # Produce plot of log-likelihood
    theta_vals <- seq(theta_range[1], theta_range[2], len = 200)
    loglik_vals <- do.call(loglik, c(list(theta_vals), user_args))
    graphics::plot(theta_vals, loglik_vals, type = "l", lwd = 2, axes = FALSE,
                   ann = FALSE)
    graphics::title(xlab = expression(theta), cex.lab = 1.5)
    graphics::title(ylab = expression(ln*L(theta)), cex.lab = 1.5)
    u <- graphics::par("usr")
    # Value of loglik at theta0
    loglik_at_theta0 <- do.call(loglik, c(list(theta0), user_args))
    yaxis_ticks <- pretty(loglik_vals)
    cond1 <- abs(yaxis_ticks - loglik_at_theta0) < (u[4] - u[3]) / 10
    cond2 <- abs(yaxis_ticks - loglik_at_mle) < (u[4] - u[3]) / 10
    if (test_stat == "Wilks" || test_stat == "all" || test_stat == "none") {
      yaxis_ticks <- yaxis_ticks[!cond1 & !cond2]
    }
    if (theta0 > theta_mle) {
      graphics::axis(2, at = yaxis_ticks)
      graphics::axis(4, at = pretty(loglik_vals))
      graphics::axis(1, mgp = c(3, 1.25, 0))
    } else {
      graphics::axis(2, at = pretty(loglik_vals))
      graphics::axis(4, at = yaxis_ticks)
      graphics::axis(1, mgp = c(3, 1.25, 0))
    }
    # Calculate score statistic
    if (is.null(alg_score)) {
      for_grad <- c(list(func = loglik, x = theta0), user_args)
      grad_at_theta0 <- do.call(numDeriv::grad, for_grad)
    } else {
      for_alg_score <- c(list(theta0), user_args)
      grad_at_theta0 <- do.call(alg_score, for_alg_score)
    }
    # Add line for theta0
    if (test_stat == "none") {
      if (theta0 > theta_mle) {
        graphics::segments(theta0, u[3], theta0, loglik_at_theta0, lty = 2,
                           lwd = 2)
        graphics::axis(1, at = theta0, tick = FALSE,
                       labels = expression(theta[0]), mgp = c(3, 0.5, 0))
        graphics::segments(u[1], loglik_at_theta0, theta0, loglik_at_theta0,
                           lty = 2, lwd = 2)
        graphics::axis(2, at = loglik_at_theta0, tick = FALSE, las = 1,
                       labels = expression(ln*L(theta[0])), mgp = c(3, 0.5, 0))
      } else {
        graphics::segments(theta0, u[3], theta0, loglik_at_theta0, lty = 2,
                           lwd = 2)
        graphics::axis(1, at = theta0, tick = FALSE, las = 1,
                       labels = expression(theta[0]), mgp = c(3, 0.5, 0))
        graphics::segments(u[2], loglik_at_theta0, theta0, loglik_at_theta0,
                           lty = 2, lwd = 2)
        graphics::axis(4, at = loglik_at_theta0, tick = FALSE, las = 1,
                       labels = expression(ln*L(theta[0])), mgp = c(3, 0.5, 0))
      }
    }
    if (test_stat == "Wald" || test_stat == "all") {
      graphics::segments(theta0, u[3], theta0, loglik_at_theta0, lty = 2,
                         lwd = 2)
      graphics::axis(1, at = theta0, tick = FALSE,
                     labels = expression(theta[0]), mgp = c(3, 0.5, 0))
      graphics::segments(theta_mle, u[3], theta_mle, loglik_at_mle, lty = 2,
                         lwd = 2)
      graphics::axis(1, at = theta_mle, tick = FALSE,
                     labels = expression(hat(theta)), mgp = c(3, 0.5, 0))
      low_val <- min(loglik_vals[is.finite(loglik_vals)])
      if (abs(theta_mle - theta0) > 1e-5) {
        graphics::arrows(theta_mle, low_val, theta0, low_val, code = 3,
                         lwd = 2, xpd = TRUE, angle = 60, length = 0.1)
      }
      graphics::text(mean(c(theta_mle, theta0)), low_val, "Wald", pos = 3)
    }
    if (test_stat == "Wilks" || test_stat == "all") {
      if (theta0 > theta_mle) {
        graphics::segments(u[1], loglik_at_mle, theta_mle, loglik_at_mle,
                           lty = 2, lwd = 2)
        graphics::axis(2, at = loglik_at_mle, tick = FALSE, las = 1,
                       labels = expression(ln*L(hat(theta))),
                       mgp = c(3, 0.5, 0))
        graphics::segments(u[1], loglik_at_theta0, theta0, loglik_at_theta0,
                           lty = 2, lwd = 2)
        graphics::axis(2, at = loglik_at_theta0, tick = FALSE, las = 1,
                       labels = expression(ln*L(theta[0])), mgp = c(3, 0.5, 0))
        y_val <- mean(c(loglik_at_theta0, loglik_at_mle))
        graphics::text(theta_range[1], y_val, "Wilks", pos = 4)
      } else {
        graphics::segments(u[2], loglik_at_mle, theta_mle, loglik_at_mle,
                           lty = 2, lwd = 2)
        graphics::axis(4, at = loglik_at_mle, tick = FALSE, las = 1,
                       labels = expression(ln*L(hat(theta))),
                       mgp = c(3, 0.5, 0))
        graphics::segments(u[2], loglik_at_theta0, theta0, loglik_at_theta0,
                           lty = 2, lwd = 2)
        graphics::axis(4, at = loglik_at_theta0, tick = FALSE, las = 1,
                       labels = expression(ln*L(theta[0])), mgp = c(3, 0.5, 0))
        y_val <- mean(c(loglik_at_theta0, loglik_at_mle))
        graphics::text(theta_range[2], y_val, "Wilks", pos = 2)
      }
      if (abs(loglik_at_mle - loglik_at_theta0) > 1e-5) {
        if (theta0 > theta_mle) {
          graphics::arrows(theta_range[1], loglik_at_theta0, theta_range[1],
                         loglik_at_mle, code = 3, lwd = 2, xpd = TRUE,
                         angle = 60, length = 0.1)
        } else {
          graphics::arrows(theta_range[2], loglik_at_theta0, theta_range[2],
                           loglik_at_mle, code = 3, lwd = 2, xpd = TRUE,
                           angle = 60, length = 0.1)
        }
      }
    }
    if (test_stat == "score" || test_stat == "all") {
      my_a <- loglik_at_theta0 - grad_at_theta0 * theta0
      my_b <- grad_at_theta0
      graphics::abline(a = my_a, b = my_b, lwd = 2)
      graphics::segments(theta0, u[3], theta0, loglik_at_theta0, lty = 2,
                         lwd = 2)
      graphics::axis(1, at = theta0, tick = FALSE,
                     labels = expression(theta[0]), mgp = c(3, 0.5, 0))
      if (abs(theta0 - theta_mle) < 1e-6) {
        graphics::text(theta0, loglik_at_theta0, "score", pos = 3, offset = 0.5,
                       xpd = TRUE)
      } else if (theta0 > theta_mle) {
        graphics::text(theta0, loglik_at_theta0, "score", pos = 4, offset = 1,
                       xpd = TRUE)
      } else if (theta0 < theta_mle) {
        graphics::text(theta0, loglik_at_theta0, "score", pos = 2, offset = 1,
                       xpd = TRUE)
      }
    }
    if (perform_tests == "yes") {
      # Calculate the values of the test statistics
      if (is.null(alg_obs_info)) {
        for_optimHess <- c(list(fn = loglik, par = theta0), user_args)
        obs_info_at_theta0 <- -do.call(optimHess, for_optimHess)
      } else {
        for_alg_obs_info <- c(list(theta0), user_args)
        obs_info_at_theta0 <- do.call(alg_obs_info, for_alg_obs_info)
      }
      wald <- (theta_mle - theta0) ^ 2 * obs_info_at_mle
      wilks <- 2 * (loglik_at_mle - loglik_at_theta0)
      # Avoid very slightly negative Wilks statistics owing to very slightly
      # inaccurate MLE returned by optim
      wilks <- max(wilks, 0)
      score <- grad_at_theta0 ^2 / obs_info_at_theta0
      test_stats <- c(wald, wilks, score)
      p_values <- stats::pchisq(test_stats, 1, lower.tail = FALSE)
      leg1 <- c("", "Wald", "Wilks", "score")
      leg2 <- c("statistic", signif(test_stats, 2))
      leg3 <- c("p-value", signif(p_values, 2))
      if (theta0 > theta_mle) {
        graphics::legend("bottomleft", ncol = 3, legend = c(leg1, leg2, leg3))
      } else {
        graphics::legend("bottomright", ncol = 3, legend = c(leg1, leg2, leg3))
      }
    }
    graphics::par(old_par)
  })
  return(invisible(panel))
}
