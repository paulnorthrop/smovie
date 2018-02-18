#' Fisher's transformation of the Pearson produce moment correlation
#' coefficient
#'
#' Density, distribution function, quantile function and random generator
#' for the distribution of Fisher's transformation of Pearson's product
#' moment correlation, based on a random sample from a bivariate normal
#' distribution
#'
#' @param x,q Numeric vectors of quantiles.
#' @param p A numeric vector of probabilities in [0,1].
#' @param loc,scale,shape Numeric vectors.
#'   Location, scale and shape parameters.
#'   All elements of \code{scale} must be positive.
#' @param n Numeric scalar.  The number of observations to be simulated.
#'   If \code{length(n) > 1} then \code{length(n)} is taken to be the number
#'   required.
#' @param log,log.p A logical scalar; if TRUE, probabilities p are given as
#'   log(p).
#' @param lower.tail A logical scalar.  If TRUE (default), probabilities
#'   are P[X <= x], otherwise, P[X > x].
#' @param m A numeric scalar.  The distribution is reparameterised by working
#'  with the GEV(\code{loc, scale, shape}) distribution function raised to the
#'  power \code{m}.  See \strong{Details}.
#' @seealso \code{\link[SuppDists]{Pearson}} for dpqr functions for the
#'   untransformed Pearson produce moment correlation coefficient.
#' @examples
#' dFPearson(x = 0, N = 10)
#' @name FPearson
NULL
## NULL

#' @rdname FPearson
#' @export
dFPearson <- function (x, N, rho = 0.0, log = FALSE) {
  if (any(rho <= -1) | any(rho >= 1)) {
    stop("invalid rho: rho must be in (-1, 1)")
  }
  if (N < 4) {
    stop("invalid N: N must be at least 4")
  }
  max_len <- max(length(x), length(N), length(rho))
  x <- rep_len(x, max_len)
  N <- rep_len(N, max_len)
  rho <- rep_len(rho, max_len)
  r <- exp(2 * x)
  r <- (r - 1) / (r + 1)
  d <- SuppDists::dPearson(x = r, N = N, rho = rho) * (1 - r ^ 2)
  if (log) {
    d <- log(d)
  }
  return(d)
}
