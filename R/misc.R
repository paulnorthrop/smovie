is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}

# For rpanel::rp.tkrplot()

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

# Set the parameters of distributions
# For discrete(), continuous(), clt() and ett()

set_fun_args <- function(distn, dfun, fun_args, params,
        for_continuous = FALSE) {
  # Get the names of the parameters
  par_names <- names(formals(dfun))
  to_remove <- which(is.element(par_names, c("x", "log")))
  par_names <- par_names[-to_remove]
  # Set any parameters of the distributional functions specified in params
  params_names <- names(params)
  is_par_name <- is.element(params_names, par_names)
  fun_args <- params[is_par_name]
  # If a name was not supplied (user function supplied for distn) the return
  if (distn == "user") {
    return(fun_args)
  }
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
  if (distn == "gp" || distn == "gev") {
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
    if (is.null(fun_args$ncp)) {
      fun_args$ncp <- 0
    }
    return(fun_args)
  }
  if (distn == "gamma") {
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 2
    }
    if (!for_continuous) {
      if (!is.null(fun_args$scale)) {
        fun_args$rate <- 1 / fun_args$scale
        fun_args$scale <- NULL
      }
    }
    if (is.null(fun_args$rate) & is.null(fun_args$scale)) {
      fun_args$rate <- 1
    }
    # Put the arguments back into the usual order
    if (!is.null(fun_args$scale)) {
      n_names <- length(names(fun_args))
      which_shape <- which(names(fun_args) == "shape")
      fun_args <- fun_args[c(which_shape, (1:n_names)[-which_shape])]
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
      fun_args$df <- 3
    }
    if (is.null(fun_args$ncp)) {
      fun_args$ncp <- 0
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
    if (is.null(fun_args$ncp)) {
      fun_args$ncp <- 0
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
  if (distn == "gev") {
    if (is.null(fun_args$shape)) {
      fun_args$shape <- 0.2
    }
    if (is.null(fun_args$loc)) {
      fun_args$loc <- 0
    }
    if (is.null(fun_args$scale)) {
      fun_args$scale <- 1
    }
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
    # Put the arguments back into the usual order
    if (!is.null(fun_args$mu)) {
      n_names <- length(names(fun_args))
      which_size <- which(names(fun_args) == "size")
      fun_args <- fun_args[c(which_size, (1:n_names)[-which_size])]
    }
    return(fun_args)
  }
  if (distn == "hypergeometric") {
    if (is.null(fun_args$m)) {
      fun_args$m <- 10
    }
    if (is.null(fun_args$n)) {
      fun_args$n <- 7
    }
    if (is.null(fun_args$k)) {
      fun_args$k <- 8
    }
    return(fun_args)
  }
}

# Set the limits of the horizontal axes for the top plots for clt() and ett()

set_top_range <- function(distn, p_vec, fun_args, qfun) {
  for_qfun <- c(list(p = p_vec), fun_args)
  top_range <- do.call(qfun, for_qfun)
  if (is.element(distn , c("exponential", "gamma", "chi-squared",
                           "lognormal", "f", "weibull", "geometric",
                           "negative binomial"))) {
    top_range[1] <- 0
    return(top_range)
  }
  if (distn == "binomial") {
    top_range[1] <- 0
    top_range[2] <- fun_args$size
    return(top_range)
  }
  if (distn == "hypergeometric") {
    top_range[1] <- 0
    top_range[2] <- fun_args$k
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
  if (is.element(distn , c("normal", "t", "cauchy", "poisson"))) {
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

# Set the positions of the legends in the plots for clt() and ett()

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
           "binomial" = "right",
           "geometric" = "right",
           "hypergeometric" = "right",
           "negative binomial" = "right",
           "poisson" = "right",
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
           "binomial" = "topright",
           "geometric" = "topright",
           "hypergeometric" = "topleft",
           "negative binomial" = "topright",
           "poisson" = "topright",
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

# Allowable ranges of the parameters for discrete() and continuous()

parameter_range <- function(distn, fun_args, ep, n_pars) {
  if (distn == "user") {
    par_step <- rep(list(c(NA, NA)), n_pars)
    names(par_step) <- names(fun_args)
    return(par_step)
  }
  if (distn == "binomial") {
    size <- c(1, NA)
    prob <- c(0, 1)
    return(list(size = size, prob = prob))
  }
  if (distn == "poisson") {
    lambda <- c(0, Inf)
    return(list(lambda = lambda))
  }
  if (distn == "geometric") {
    prob <- c(ep, 1)
    return(list(prob = prob))
  }
  if (distn == "negative binomial") {
    if (!is.null(fun_args$prob)) {
      size <- c(0, NA)
      prob <- c(ep, 1)
      return(list(size = size, prob = prob))
    } else {
      size <- c(0, NA)
      mu <- c(ep, NA)
      return(list(size = size, prob = prob))
    }
  }
  if (distn == "hypergeometric") {
    m <- c(1, NA)
    n <- c(1, NA)
    k <- c(1, NA)
    return(list(m = m, n = n, k = k))
  }
  if (distn == "normal") {
    mu <- c(NA, NA)
    sd <- c(ep, NA)
    return(list(mean = mu, sd = sd))
  }
  if (distn == "beta") {
    shape1 <- c(ep, NA)
    shape2 <- c(ep, NA)
    ncp <- c(0, NA)
    return(list(shape1 = shape1, shape2 = shape2, ncp = ncp))
  }
  if (distn == "cauchy") {
    location <- c(NA, NA)
    scale <- c(ep, NA)
    return(list(location = location, scale = scale))
  }
  if (distn == "chi-squared") {
    df <- c(ep, NA)
    ncp <- c(0, NA)
    return(list(df = df, ncp = ncp))
  }
  if (distn == "exponential") {
    rate <- c(ep, NA)
    return(list(rate = rate))
  }
  if (distn == "f") {
    df1 <- c(ep, NA)
    df2 <- c(ep, NA)
    ncp <- c(0, NA)
    return(list(df1 = df1, df2 = df2, ncp = ncp))
  }
  if (distn == "gamma") {
    if (!is.null(fun_args$scale)) {
      shape <- c(ep, NA)
      scale <- c(ep, NA)
      return(list(shape = shape, scale = scale))
    } else {
      shape <- c(ep, NA)
      rate <- c(ep, NA)
      return(list(shape = shape, rate = rate))
    }
  }
  if (distn == "gev" | distn == "gp") {
    loc <- c(NA, NA)
    scale <- c(ep, NA)
    shape <- c(NA, NA)
    return(list(loc = loc, scale = scale, shape = shape))
  }
  if (distn == "lognormal") {
    meanlog <- c(NA, NA)
    sdlog <- c(ep, NA)
    return(list(meanlog = meanlog, sdlog = sdlog))
  }
  if (distn == "t") {
    df <- c(ep, NA)
    ncp <- c(-37.62, 37.62)
    return(list(df = df, ncp = ncp))
  }
  if (distn == "uniform") {
    mid_range <- (fun_args$min + fun_args$max) / 2
    min <- c(NA,  mid_range - 0.05)
    max <- c(mid_range + 0.05, NA)
    return(list(min = min, max = max))
  }
  if (distn == "weibull") {
    shape <- c(ep, NA)
    scale <- c(ep, NA)
    return(list(shape = shape, scale = scale))
  }
}

# Increments of changes in parameter values for discrete() and continuous()

parameter_step <- function(distn, fun_args, n_pars) {
  if (distn == "user") {
    par_step <- rep(list(0.1), n_pars)
    names(par_step) <- names(fun_args)
    return(par_step)
  }
  if (distn == "binomial") {
    return(list(size = 1, prob = 0.1))
  }
  if (distn == "negative binomial") {
    if (!is.null(fun_args$prob)) {
      return(list(size = 1, prob = 0.1))
    } else {
      return(list(size = 1, mu = 1))
    }
  }
  if (distn == "poisson") {
    return(list(lambda = 0.5))
  }
  if (distn == "geometric") {
    return(list(prob = 0.1))
  }
  if (distn == "hypergeometric") {
    return(list(m = 1, n = 1, k = 1))
  }
  if (distn == "normal") {
    return(list(mean = 1, sd = 0.25))
  }
  if (distn == "beta") {
    return(list(shape1 = 0.5, shape2 = 0.5, ncp = 1))
  }
  if (distn == "cauchy") {
    return(list(location = 1, scale = 0.25))
  }
  if (distn == "chi-squared") {
    return(list(df = 1, ncp = 1))
  }
  if (distn == "exponential") {
    return(list(rate = 0.25))
  }
  if (distn == "f") {
    return(list(df1 = 1, df2 = 1, ncp = 1))
  }
  if (distn == "gamma") {
    if (!is.null(fun_args$scale)) {
      return(list(shape = 0.25, scale = 0.25))
    } else {
      return(list(shape = 0.25, rate = 0.25))
    }
  }
  if (distn == "gev" || distn == "gp") {
    return(list(loc = 1, scale = 0.25, shape = 0.1))
  }
  if (distn == "lognormal") {
    return(list(meanlog = 0.25, sdlog = 0.25))
  }
  if (distn == "t") {
    return(list(df = 1, ncp = 1))
  }
  if (distn == "uniform") {
    return(list(min = 0.25, max = 0.25))
  }
  if (distn == "weibull") {
    return(list(shape = 0.25, scale = 0.25))
  }
}

# Set the support of discrete distributions for discrete()

variable_support <- function(distn, fun_args, qfun, pmf_or_cdf, p_vec){
  if (distn == "user") {
    for_qfun <- c(list(p = p_vec), fun_args)
    var_range <- do.call(qfun, for_qfun)
    return(var_range[1]:var_range[2])
  }
  if (distn == "binomial") {
    return(0:fun_args$size)
  }
  if (distn == "poisson") {
    if (fun_args$lambda == 0) {
      if (pmf_or_cdf == "pmf") {
        return(0:2)
      } else {
        return(-1:1)
      }
    } else {
      for_qfun <- c(list(p = p_vec), fun_args)
      var_range <- do.call(qfun, for_qfun)
      return(var_range[1]:var_range[2])
    }
  }
  if (distn == "negative binomial") {
    if (fun_args$size == 0) {
      if (pmf_or_cdf == "pmf") {
        return(0:2)
      } else {
        return(-1:1)
      }
    } else {
      for_qfun <- c(list(p = p_vec), fun_args)
      var_range <- do.call(qfun, for_qfun)
      return(var_range[1]:var_range[2])
    }
  }
  if (distn == "geometric") {
    for_qfun <- c(list(p = p_vec), fun_args)
    var_range <- do.call(qfun, for_qfun)
    return(var_range[1]:var_range[2])
  }
  if (distn == "hypergeometric") {
    return(0:fun_args$k)
  }
}

# Set the support of continuous distributions for continuous()

variable_range <- function(distn, fun_args, qfun, p_vec){
  for_qfun <- c(list(p = p_vec), fun_args)
  return(do.call(qfun, for_qfun))
}

# Vector of cdf probabilities for use in continous()

set_p_vec <- function(distn) {
  if (distn %in% c("normal", "user", "chi-squared", "f", "gamma", "gev",
                   "lognormal", "t", "weibull")) {
    return(c(0.001, 0.999))
  }
  if (distn %in% c("exponential", "gp")) {
    return(c(0, 0.999))
  }
  if (distn == "cauchy") {
    return(c(0.05, 0.95))
  }
  if (distn %in% c("beta", "uniform")) {
    return(c(0, 1))
  }
}

# Allow standard d/p/q/rxxx abbreviations from stats::Distributions for
# discrete() and continuous()

recognise_stats_abbreviations <- function(distn) {
  if (distn == "binom") {
    return("binomial")
  }
  if (distn == "geom") {
    return("geometric")
  }
  if (distn == "hyper") {
    return("hypergeometric")
  }
  if (distn == "nbinom") {
    return("negative binomial")
  }
  if (distn == "pois") {
    return("poisson")
  }
  if (distn == "norm") {
    return("normal")
  }
  if (distn == "exp") {
    return("exponential")
  }
  if (distn == "lnorm") {
    return("lognormal")
  }
  if (distn == "unif") {
    return("uniform")
  }
  if (distn == "chisq") {
    return("chi-squared")
  }
  return(distn)
}

# Merge list1 and list2.  If a component exists in both list1 and list2 then
# use the component in list2

merge_lists <- function(list1, list2) {
  names1 <- names(list1)
  names2 <- names(list2)
  # Find all the names
  all_names <- unique(c(names1, names2))
  in1 <- is.element(all_names, names1)
  in2 <- is.element(all_names, names2)
  # If in2 is TRUE use list2 value, otherwise list1
  merged_list <- ifelse(in2, list2, list1)
  names(merged_list) <- all_names
  return(merged_list)
}

