#' Fisher's transformation of the Pearson product moment correlation
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
#' @details These functions rely on the \code{\link[SuppDists]{Pearson}}
#'   functions in the SuppDists package.  SuppDists must be installed in order
#'   for these functions to work.
#' @seealso \code{\link[SuppDists]{Pearson}} in the SuppDists package for
#'   dpqr functions for the untransformed Pearson produce moment correlation
#'   coefficient.
#' @examples
#' dFPearson(-1:1, N = 10)
#' dFPearson(0, N = 11:20)
#'
#' pFPearson(0.5, N = 10)
#' pFPearson(0.5, N = 10, rho = c(0, 0.3))
#'
#' qFPearson((1:9)/10, N = 10, rho = 0.2)
#' qFPearson(0.5, N = c(10, 20), rho = c(0, 0.3))
#'
#' rFPearson(6, N = 10, rho = 0.6)
#' @seealso \code{\link{correlation}}: correlation sampling distribution movie.
#' @references Fisher, R. A. (1915). Frequency distribution of the values of
#'   the correlation coefficient in samples of an indefinitely large
#'   population. \emph{Biometrika}, \strong{10}(4), 507-521.
#'   \url{http://dx.doi.org/10.2307/2331838}
#' @references  Fisher, R. A. (1921). On the "probable error" of a coefficient
#'   of correlation deduced from a small sample. \emph{Metron}, \strong{1},
#'   3-32.
#'   \url{https://digital.library.adelaide.edu.au/dspace/bitstream/2440/15169/1/14.pdf}
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
  max_len <- max(length(p), length(N), length(rho))
  p <- rep_len(p, max_len)
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
  max_len <- ifelse(length(n) > 1, length(n), n)
  N <- rep_len(N, max_len)
  rho <- rep_len(rho, max_len)
  r <- SuppDists::rPearson(n = n, N = N, rho = rho)
  x <- atanh(r)
  return(x)
}
