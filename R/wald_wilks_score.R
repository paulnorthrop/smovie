# =================================== wws =====================================

#' Wald, Wilks and Score test movie
#'
#' A movie to illustrate the nature of the Wald, Wilks and score
#' likelihood-based test statistics, for a model with a scalar unknown
#' parameter \eqn{\theta}.  The user can change the value of the parameter
#' under a simple null hypothesis and observe the effect on the test
#' statistics and (approximate) p-values associated with the tests of
#' this hypothesis against the general alternative.  The user can
#' specify their own loglikelihood or use one of two in-built examples.
#'
#' @param model A character scalar.  Name of the the distribution on which
#'   one of two in-built examples are based.
#'
#'   If \code{model = "normal"} then the setting is a random sample of
#'   size \code{n} from a normal distribution with unknown mean
#'   \code{mu} = \eqn{\theta} and known standard deviation \code{sigma}.
#'
#'   If \code{model = "normal"} then the setting is a random sample from a
#'   Bernoulli distribution with unknown success probability
#'   \eqn{\theta}.
#'
#'   The behaviour of these examples can be controlled using arguments
#'   supplied via \code{...}.  In particular, the data can be supplied
#'   using \code{data}.  If \code{model = "norm"} then \code{n}, \code{mu},
#'   and \code{sigma} can also be chosen.
#'   The default cases for these examples are:
#'   \itemize{
#'     \item{\code{model = "norm"}: }{\code{n} = 10, \code{mu} = 0,
#'       \code{sigma} = 1 and \code{data} contains a sample of
#'       a sample of size \code{n} simulated, using \code{\link[stats]{rnorm}},
#'       from a normal distribution with mean \code{mu} and standard deviation
#'       \code{sigma}.}
#'     \item{\code{model = "binom"}: }{\code{data = c(7, 13)}, that is,
#'       7 successes and 13 failures observed in 20 trials.  For the purposes
#'       of this movie there must be at least one success and at least one
#'       failure.}
#'   }
#' @param loglik An R function, vectorised with respect to its first argument,
#'   that returns the value of the log-likelihood (up to an additive constant).
#'   The movie will not work if the observed information is not finite at the
#'   maximum likelihood estimate.
#' @param theta_range A numeric vector of length 2.  The range of values of
#'   \eqn{\theta} over which to plot the log-likelihood.
#'   If \code{theta_range} is not supplied then the argument \code{mult}
#'   detailed below is used to set the range automatically.
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
#' @param alg_score A R function that returns the score function, that is,
#'   the derivative of \code{loglik} with respect to \eqn{\theta}.
#' @param alg_obs_info A R function that returns the observed information
#'   that is, the negated second derivative of \code{loglik} with respect
#'   to \eqn{\theta}.
#' @param digits An integer indicating the number of significant digits to
#'   be used in the displayed values of the test statistics and
#'   p-values.  See \code{\link{signif}}.
#' @param mult A positive numeric scalar.  If \code{theta_range} is not
#'   supplied then an interval of width 2\code{mult} standard errors centred
#'   on \code{theta_mle} is used.  If \code{model = "binom"} then
#'   \code{theta_range} is truncated to (0,1) if necessary.
#' @param ... Additional arguments to be passed to \code{loglik},
#'   \code{alg_score} and \code{alg_obs_info} if \code{loglik} is supplied,
#'   or to functions functions relating to the in-built examples otherwise.
#'   See the description of \code{model} above for details.
#' @details Add details.
#' @return Nothing is returned, only the animation is produced.
#' @examples
#' \dontrun{
#' # N(theta, 1) example, test statistics equivalent
#' wws(theta0 = 0.8)
#'
#' # binomial(20, theta) example, test statistics similar
#' wws(theta0 = 0.5, model = "binom")
#'
#' # binomial(20, theta) example, test statistic rather different
#' # for theta0 distant from theta_mle
#' wws(theta0 = 0.9, model = "binom", data = c(19, 1), theta_range = c(0.1, 0.99))
#'
#' # binomial(2000, theta) example, test statistics very similar
#' wws(theta0 = 0.5, model = "binom", data = c(1000, 1000))
#'
#' set.seed(47)
#' x <- rnorm(10)
#' wws(theta0 = 0.2, model = "norm", theta_range = c(-1, 1))
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
wws <- function(model = c("norm", "binom"),
                theta_range = NULL,
                theta0 = if (!is.null(theta_range))
                  mean(theta_range) else NULL,
                delta_theta0 = if (!is.null(theta_range))
                  abs(diff(theta_range)) / 20 else NULL,
                theta_mle = NULL,
                loglik = NULL, alg_score = NULL, alg_obs_info = NULL,
                digits = 3, mult = 3, ...) {
  model <- match.arg(model)
  user_args <- list(...)
  # If loglik is not supplied then use the log-likelihood implied by model
  if (is.null(loglik)) {
    if (model == "binom") {
      loglik <- function(p, n_success, n_failure) {
        return(n_success * log(p) + n_failure * log(1 - p))
      }
      alg_score <- function(p, n_success, n_failure) {
        return(n_success / p - n_failure / (1 - p))
      }
      alg_obs_info <- function(p, n_success, n_failure) {
        return(n_success / p ^ 2 + n_failure / (1 - p) ^ 2)
      }
      if (is.null(user_args$data)) {
        user_args$n_success <- 7
        user_args$n_failure <- 13
      } else {
        if (min(user_args$data) == 0) {
          stop("min(data) must be positive for this movie to work")
        }
        user_args$n_success <- user_args$data[1]
        user_args$n_failure <- user_args$data[2]
        # Remove data from user_args because it isn't an argument of loglik
        user_args$data <- NULL
      }
      n <- user_args$n_success + user_args$n_failure
      theta_mle <- user_args$n_success / n
      eps <- 1e-6
      if (is.null(theta_range)) {
        se_theta <- sqrt(theta_mle * (1 - theta_mle) / n)
        theta_range <- theta_mle + c(-1, 1) * mult * se_theta
      }
      theta_range[2] <- min(1  - eps, theta_range[2])
      theta_range[1] <- max(eps, theta_range[1])
    } else if (model == "norm") {
      loglik <- function(mu, data, sigma, n) {
        sx2 <- sum(data ^ 2)
        sumx <- sum(data)
        return(-(sx2 - 2 * sumx * mu + n * mu ^ 2) / sigma ^ 2 / 2)
      }
      alg_score <- function(mu, data, sigma, n) {
        return(n * (mean(data) - mu) / sigma ^ 2)
      }
      alg_obs_info <- function(mu, data, sigma, n) {
        return(n / sigma ^ 2)
      }
      if (is.null(user_args$mu)) {
        sim_mu <- 0
      } else {
        sim_mu <- user_args$mu
        # Remove mu from user_args because mu is an argument of loglik
        user_args$mu <- NULL
      }
      if (is.null(user_args$sigma)) {
        user_args$sigma <- 1
      }
      if (is.null(user_args$n)) {
        user_args$n <- 10
      }
      if (is.null(user_args$data)) {
        user_args$data <- stats::rnorm(user_args$n, mean = sim_mu,
                                       sd = user_args$sigma)
      }
      theta_mle <- mean(user_args$data)
      if (is.null(theta_range)) {
        se_theta <- user_args$sigma / sqrt(user_args$n)
        theta_range <- theta_mle + c(-1, 1) * mult * se_theta
      }
    }
  }
  theta_range <- sort(theta_range)
  # Make sure that theta0 is included on the initial plot
  if (!is.null(theta0)) {
    theta_range[2] <- max(theta0, theta_range[2])
    theta_range[1] <- min(theta0, theta_range[1])
  }
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
    for_optim <- c(list(par = theta0, fn = obfn, lower = lower,
                        upper = upper), user_args)
    temp <- suppressWarnings(do.call(stats::optim, for_optim))
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
  if (is.na(obs_info_at_mle) | is.infinite(obs_info_at_mle)) {
    stop("The observed information is not finite at the MLE")
  }
  test_stat <- "none"
  perform_tests <- "no"
  # Create buttons for movie
  wws_panel <- rpanel::rp.control("Change theta0",
                                 loglik = loglik, theta_range = theta_range,
                                 theta0 = theta0, user_args = user_args,
                                 test_stat = "none",
                                 perform_tests = "no", theta_mle = theta_mle,
                                 loglik_at_mle = loglik_at_mle,
                                 alg_score = alg_score,
                                 alg_obs_info = alg_obs_info,
                                 obs_info_at_mle = obs_info_at_mle,
                                 digits = digits)
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
    if (test_stat == "Wilks" || test_stat == "all") {
      yaxis_labels <- pretty(loglik_vals)
      yaxis_labels[cond1 | cond2] <- ""
    } else if (test_stat == "none") {
      yaxis_labels <- pretty(loglik_vals)
      yaxis_labels[cond1] <- ""
    } else {
      yaxis_labels <- pretty(loglik_vals)
    }
    if (theta0 > theta_mle) {
      graphics::axis(2, at = pretty(loglik_vals), labels = yaxis_labels)
      graphics::axis(4, at = pretty(loglik_vals))
      graphics::axis(1, mgp = c(3, 1.25, 0))
    } else {
      graphics::axis(2, at = pretty(loglik_vals))
      graphics::axis(4, at = pretty(loglik_vals), labels = yaxis_labels)
      graphics::axis(1, mgp = c(3, 1.25, 0))
    }
    graphics::box(bty = "u")
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
        graphics::segments(theta_mle, low_val, theta0, low_val, lwd = 2,
                           xpd = TRUE)
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
          graphics::segments(theta_range[1], loglik_at_theta0, theta_range[1],
                             loglik_at_mle, lwd = 2, xpd = TRUE)
        } else {
          graphics::segments(theta_range[2], loglik_at_theta0, theta_range[2],
                             loglik_at_mle, lwd = 2, xpd = TRUE)
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
      getCurrentAspect <- function() {
        uy <- diff(graphics::grconvertY(1:2, "user", "inches"))
        ux <- diff(graphics::grconvertX(1:2, "user", "inches"))
        return(uy/ux)
      }
      asp <- getCurrentAspect()
      graphics::text(theta0, loglik_at_theta0, "score", pos = 3, offset = 1,
                     srt = 180 / pi * atan(grad_at_theta0 * asp), xpd = TRUE)
    }
    if (perform_tests == "yes") {
      # Calculate the values of the test statistics
      if (is.null(alg_obs_info)) {
        for_optimHess <- c(list(fn = loglik, par = theta0), user_args)
        obs_info_at_theta0 <- -do.call(stats::optimHess, for_optimHess)
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
      leg2 <- c("stat", signif(test_stats, digits))
      leg3 <- c("p-value", signif(p_values, digits))
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
