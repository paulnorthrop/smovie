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
#' @param N Numeric vector.  Number of observations, (N > 3).
#' @param rho Numeric vector.  Population correlations, (-1 < rho < 1).
#' @param n Numeric scalar.  The number of observations to be simulated.
#'   If \code{length(n) > 1} then \code{length(n)} is taken to be the number
#'   required.
#' @param log,log.p A logical scalar; if TRUE, probabilities p are given as
#'   log(p).
#' @param lower.tail A logical scalar.  If TRUE (default), probabilities
#'   are P[X <= x], otherwise, P[X > x].
#' @seealso \code{\link[SuppDists]{Pearson}} for dpqr functions for the
#'   untransformed Pearson produce moment correlation coefficient.
#' @examples
#' dFPearson(x = -1:1, N = 10)
#' dFPearson(x = 0, N = 11:20)
#'
#' pFPearson(q = 0.5, N = 10)
#' pFPearson(q = 0.5, N = 10, rho = c(0, 0.3))
#' @name FPearson
NULL
## NULL

# ------------------------------- dFPearson -----------------------------------

#' @rdname FPearson
#' @export
dFPearson <- function (x, N, rho = 0.0, log = FALSE) {
  if (any(rho <= -1) | any(rho >= 1)) {
    stop("invalid rho: rho must be in (-1, 1)")
  }
  if (any(N < 4)) {
    stop("invalid N: N must be at least 4")
  }
  max_len <- max(length(x), length(N), length(rho))
  x <- rep_len(x, max_len)
  N <- rep_len(N, max_len)
  rho <- rep_len(rho, max_len)
  r <- tanh(x)
  d <- SuppDists::dPearson(x = r, N = N, rho = rho) * (1 - r ^ 2)
  if (log) {
    d <- log(d)
  }
  return(d)
}

# ------------------------------- pFPearson -----------------------------------

#' @rdname FPearson
#' @export
pFPearson <- function(q, N, rho = 0.0, lower.tail = TRUE, log.p = FALSE) {
  if (any(rho <= -1) | any(rho >= 1)) {
    stop("invalid rho: rho must be in (-1, 1)")
  }
  if (any(N < 4)) {
      stop("invalid N: N must be at least 4")
  }
  max_len <- max(length(q), length(N), length(rho))
  q <- rep_len(q, max_len)
  N <- rep_len(N, max_len)
  rho <- rep_len(rho, max_len)
  r <- tanh(q)
  p <- SuppDists::pPearson(q = r, N = N, rho = rho, lower.tail = lower.tail,
                           log.p = log.p)
  return(p)
}

# ------------------------------- qFPearson -----------------------------------

#' @rdname FPearson
#' @export
qFPearson <- function(p, N, rho = 0.0, lower.tail = TRUE, log.p = FALSE) {
  if (any(rho <= -1) | any(rho >= 1)) {
    stop("invalid rho: rho must be in (-1, 1)")
  }
  if (any(N < 4)) {
      stop("invalid N: N must be at least 4")
  }
  max_len <- max(length(q), length(N), length(rho))
  q <- rep_len(q, max_len)
  N <- rep_len(N, max_len)
  rho <- rep_len(rho, max_len)
  r <- SuppDists::qPearson(p = p, N = N, rho = rho, lower.tail = lower.tail,
                           log.p = log.p)
  x <- atanh(r)
  return(x)
}

# ------------------------------- rFPearson -----------------------------------

#' @rdname FPearson
#' @export
rFPearson <- function(n, N, rho = 0.0, lower.tail = TRUE, log.p = FALSE) {
  if (any(rho <= -1) | any(rho >= 1)) {
    stop("invalid rho: rho must be in (-1, 1)")
  }
  if (any(N < 4)) {
    stop("invalid N: N must be at least 4")
  }
  max_len <- max(length(q), length(N), length(rho))
  q <- rep_len(q, max_len)
  N <- rep_len(N, max_len)
  rho <- rep_len(rho, max_len)
  r <- SuppDists::rPearson(n = n, N = N, rho = rho)
  x <- atanh(r)
  return(x)
}
