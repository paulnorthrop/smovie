context("FPearson functions")

# We check the functions pFPearson, qFPearson and rFPearson.

# Set a tolerance for the comparison of the simulated values
my_tol <- 1e-5

# 1. Check that calling qFPearson with probabilities p and then calling pgev
#    with the results gets us back to the initial probabilities.

pqFPearson_test_fn <- function(x, p) {
  N <- x[1]
  rho <- x[2]
  qs <- qFPearson(p = p, N = N, rho = rho)
  ps <- pFPearson(qs, N = N, rho = rho)
  return(list(p = p, ps = ps))
}

test_function <- function(x, test_string) {
  testthat::test_that(test_string, {
    testthat::expect_equal(x$p, x$ps, tolerance = my_tol)
  })
}

ep <- 1e-10
N_check <- c(10, 20, 30)
rho_check <- c(-0.99, -0.5, -0.1, -ep, 0, ep, 0.1, 0.5, 0.99)
par_vals <- cbind(N_check, rho_check)
p_vals <- c(0.01, 0.1, 0.5, 0.9, 0.99)
for (i in 1:nrow(par_vals)) {
  test_string <- paste("p and q, FPearson (rho, N) = ", par_vals[i, ])
  x <- pqFPearson_test_fn(x = par_vals[i, ], p = p_vals)
  test_function(x, test_string)
}

# 2. Similar to 1. but now generate the quantiles using rFPearson().

seed <- 19022018
set.seed(seed)

rpqFPearson_test_fn <- function(x, p) {
  N <- x[1]
  rho <- x[2]
  qs_in <- rFPearson(n = 5, N = N, rho = rho)
  ps <- pFPearson(qs, N = N, rho = rho)
  qs_out <- qFPearson(p = p, N = N, rho = rho)
  return(list(qs_in = qs_in, qs_out = qs_out))
}

test_function <- function(x, test_string) {
  testthat::test_that(test_string, {
    testthat::expect_equal(x$qs_in, x$qs_out, tolerance = my_tol)
  })
}

ep <- 1e-10
N_check <- c(10, 20, 30)
rho_check <- c(-0.99, -0.5, -0.1, -ep, 0, ep, 0.1, 0.5, 0.99)
par_vals <- cbind(N_check, rho_check)
p_vals <- c(0.01, 0.1, 0.5, 0.9, 0.99)
for (i in 1:nrow(par_vals)) {
  test_string <- paste("p, q and r, FPearson (rho, N) = ", par_vals[i, ])
  x <- pqFPearson_test_fn(x = par_vals[i, ], p = p_vals)
  test_function(x, test_string)
}

