is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}

set_scales <- function(hscale, vscale) {
  if (is.na(hscale)) {
    if (.Platform$OS.type == "unix") {
      hscale <- 1.4
    } else {
      hscale <- 2
    }
  }
  if (is.na(vscale)) {
    vscale <- hscale
  }
  return(list(hscale = hscale, vscale = vscale))
}

set_fun_args <- function(distn, dfun, fun_args, params) {
  # Get the names of the parameters
  par_names <- names(formals(dfun))
  to_remove <- which(is.element(par_names, c("x", "log")))
  par_names <- par_names[-to_remove]
  # Set any parameters of the distributional functions specified in params
  params_names <- names(params)
  is_par_name <- is.element(params_names, par_names)
  fun_args <- params[is_par_name]
  if (distn == "exponential") {
    if (is.null(fun_args$rate)) {
      fun_args$rate <- 1
    }
    return(fun_args)
  }
  if (distn == "uniform") {
    if (is.null(fun_args$min)) {
      fun_args$min <- 0
    }
    if (is.null(fun_args$max)) {
      fun_args$max <- 1
    }
    return(fun_args)
  }
  if (distn == "gp") {
    if (is.null(fun_args$loc)) {
      fun_args$loc <- 0
    }
    if (is.null(fun_args$scale)) {
      fun_args$scale <- 1
    }
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 0.1
    }
    return(fun_args)
  }
  if (distn == "normal") {
    if (is.null(fun_args$mean)) {
      fun_args$mean <- 0
    }
    if (is.null(fun_args$sd)) {
      fun_args$sd <- 1
    }
    return(fun_args)
  }
  if (distn == "beta") {
    if (is.null(fun_args$shape1)) {
      fun_args$shape1 <- 2
    }
    if (is.null(fun_args$shape2)) {
      fun_args$shape2 <- 2
    }
    if (is.null(fun_args$ncp)) {
      fun_args$ncp <- 0
    }
    return(fun_args)
  }
  if (distn == "t") {
    if (is.null(fun_args$df)) {
      fun_args$df <- 4
    }
    return(fun_args)
  }
  if (distn == "gamma") {
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 2
    }
    if (!is.null(fun_args$scale)) {
      fun_args$rate <- 1 / fun_args$scale
      fun_args$scale <- NULL
    }
    if (is.null(fun_args$rate) & is.null(fun_args$scale)) {
      fun_args$rate <- 1
    }
    return(fun_args)
  }
  if (distn == "lognormal") {
    if (is.null(fun_args$meanlog)) {
      fun_args$meanlog <- 0
    }
    if (is.null(fun_args$sdlog)) {
      fun_args$sdlog <- 1
    }
    return(fun_args)
  }
  if (distn == "cauchy") {
    if (is.null(fun_args$mean)) {
      fun_args$location <- 0
    }
    if (is.null(fun_args$sd)) {
      fun_args$scale <- 1
    }
    return(fun_args)
  }
  if (distn == "chi-squared") {
    if (is.null(fun_args$df)) {
      fun_args$df <- 4
    }
    return(fun_args)
  }
  if (distn == "f") {
    if (is.null(fun_args$df1)) {
      fun_args$df1 <- 4
    }
    if (is.null(fun_args$df2)) {
      fun_args$df2 <- 8
    }
    return(fun_args)
  }
  if (distn == "weibull") {
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 2
    }
    if (is.null(fun_args$scale)) {
      fun_args$scale <- 1
    }
    return(fun_args)
  }
  if (distn == "ngev") {
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 0.2
    }
    fun_args$loc <- 1 / fun_args$shape
    fun_args$scale <- 1
    return(fun_args)
  }
  if (distn == "binomial") {
    if (is.null(fun_args$size)) {
      fun_args$size <- 10
    }
    if (is.null(fun_args$prob)) {
      fun_args$prob <- 0.5
    }
    return(fun_args)
  }
  if (distn == "poisson") {
    if (is.null(fun_args$lambda)) {
      fun_args$lambda <- 5
    }
    return(fun_args)
  }
  if (distn == "geometric") {
    if (is.null(fun_args$prob)) {
      fun_args$prob <- 0.5
    }
    return(fun_args)
  }
  if (distn == "negative binomial") {
    if (is.null(fun_args$size)) {
      fun_args$size <- 1
    }
    if (is.null(fun_args$prob) & is.null(fun_args$mu)) {
      fun_args$prob <- 0.5
    }
    if (!is.null(fun_args$mu)) {
      n_names <- length(names(fun_args))
      which_size <- which(names(fun_args) == "size")
      print(n_names)
      print(which_size)
      fun_args <- fun_args[c(which_size, (1:n_names)[-which_size])]
    }
    print(fun_args)
    return(fun_args)
  }
}

set_top_range <- function(distn, p_vec, fun_args, qfun) {
  for_qfun <- c(list(p = p_vec), fun_args)
  top_range <- do.call(qfun, for_qfun)
  if (is.element(distn , c("exponential", "gamma", "chi-squared",
                           "lognormal", "f", "weibull"))) {
    top_range[1] <- 0
    return(top_range)
  }
  if (distn == "uniform") {
    top_range[1] <- fun_args$min
    top_range[2] <- fun_args$max
    return(top_range)
  }
  if (distn == "gp") {
    top_range[1] <- fun_args$loc
    if (fun_args$shape < 0) {
      top_range[2] <- fun_args$loc - fun_args$scale / fun_args$shape
    }
    return(top_range)
  }
  if (is.element(distn , c("normal", "t", "cauchy"))) {
    return(top_range)
  }
  if (distn == "beta") {
    if (fun_args$shape1 < 1) {
      top_range[1] <- 0.01
    } else {
      top_range[1] <- 0
    }
    if (fun_args$shape2 < 1) {
      top_range[2] <- 0.99
    } else {
      top_range[2] <- 1
    }
    return(top_range)
  }
  if (distn == "ngev") {
    top_range[2] <- 0
    return(top_range)
  }
}

set_leg_pos <- function(distn, fun_args) {
  if (distn == "gp") {
    if (fun_args$shape == -1) {
      distn <- "uniform"
    } else if (fun_args$shape < -1) {
      distn <- "gp_neg_1"
    } else if (fun_args$shape < 0) {
      distn <- "gp_neg"
    } else {
      distn <- "gp_non_neg"
    }
  }
  if (distn == "beta") {
    if (fun_args$shape2 > fun_args$shape1) {
      distn <- "beta_topright"
    } else if (fun_args$shape2 < fun_args$shape1) {
      distn <- "beta_topleft"
    } else {
      if (fun_args$shape1 >= 1) {
        distn <- "beta_topleft"
      } else {
        distn <- "beta_top"
      }
    }
  }
  top_leg_pos <-
    switch(distn,
           "exponential" = "right",
           "uniform" = "topleft",
           "gp_neg_1" = "topleft",
           "gp_neg" = "topright",
           "gp_non_neg" = "right",
           "normal" = "right",
           "beta_topleft" = "topleft",
           "beta_topright" = "topright",
           "beta_top" = "top",
           "t" = "right",
           "gamma" = "right",
           "chi-squared" = "right",
           "lognormal" = "right",
           "cauchy" = "right",
           "f" = "right",
           "weibull" = "right",
           "ngev" = "topleft"
    )
  bottom_leg_pos <-
    switch(distn,
           "exponential" = "right",
           "uniform" = "left",
           "gp_neg_1" = "left",
           "gp_neg" = "left",
           "gp_non_neg" = "right",
           "normal" = "right",
           "beta_topleft" = "topleft",
           "beta_topright" = "topleft",
           "beta_top" = "topleft",
           "t" = "right",
           "gamma" = "right",
           "chi-squared" = "right",
           "lognormal" = "right",
           "cauchy" = "right",
           "f" = "right",
           "weibull" = "right",
           "ngev" = "topleft"
    )
  return(list(top_leg_pos = top_leg_pos, bottom_leg_pos = bottom_leg_pos))
}

# Negated GEV distributions

dngev <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE){
  return(revdbayes::dgev(-x, loc = loc, scale = scale, shape = shape,
                         log = log))
}

pngev <- function(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE,
                  log.p = FALSE){
  return(revdbayes::pgev(-q, loc = loc, scale = scale, shape = shape,
              lower.tail = !lower.tail, log.p = log.p))
}

qngev <- function(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE,
                  log.p = FALSE){
  return(-revdbayes::qgev(p, loc = loc, scale = scale, shape = shape,
               lower.tail = !lower.tail, log.p = log.p))
}

rngev <- function(n, loc = 0, scale = 1, shape = 0){
  return(-revdbayes::rgev(n, loc = loc, scale = scale, shape = shape))
}

parameter_range <- function(distn, fun_args, ep) {
  if (distn == "binomial") {
    size <- c(1, NA)
    prob <- c(0, 1)
    return(cbind(size, prob))
  }
  if (distn == "poisson") {
    lambda <- matrix(c(0, Inf), 2, 1)
    return(lambda)
  }
  if (distn == "geometric") {
    prob <- matrix(c(ep, 1), 2, 1)
    return(prob)
  }
  if (distn == "negative binomial") {
    if (!is.null(fun_args$prob)) {
      size <- c(0, NA)
      prob <- c(ep, 1)
      return(cbind(size, prob))
    } else {
      size <- c(0, NA)
      mu <- c(ep, NA)
      return(cbind(size, mu))
    }
  }
}

parameter_step <- function(distn, fun_args) {
  if (distn == "binomial") {
    return(c(1, 0.1))
  }
  if (distn == "negative binomial") {
    if (!is.null(fun_args$prob)) {
      return(c(1, 0.1))
    } else {
      return(c(1, 1))
    }
  }
  if (distn == "poisson") {
    return(0.5)
  }
  if (distn == "geometric") {
    return(0.1)
  }
}

variable_support <- function(distn, fun_args, qfun){
  if (distn == "binomial") {
    return(0:fun_args$size)
  }
  if (distn == "poisson" | distn == "geometric" |
      distn == "negative binomial") {
    for_qfun <- c(list(p = c(0.001, 0.999)), fun_args)
    var_range <- do.call(qfun, for_qfun)
    return(var_range[1]:var_range[2])
  }
}
