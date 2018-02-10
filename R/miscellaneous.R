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
}

set_top_range <- function(distn, p_vec, fun_args, qfun) {
  for_qfun <- c(list(p = p_vec), fun_args)
  top_range <- do.call(qfun, for_qfun)
  if (distn == "exponential") {
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
  if (distn == "normal") {
    return(top_range)
  }
  if (distn == "beta") {
    top_range[1] <- 0
    top_range[2] <- 1
    return(top_range)
  }
}
