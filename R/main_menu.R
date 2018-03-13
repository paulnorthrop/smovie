#' Main menu for smovie movies
#'
#' Uses the template \code{\link[rpanel]{rp.cartoons}} function to produce
#' a menu panel from which any of the movies in
#' \code{\link[smovie]{smovie}} package can be launched. For greater control
#' of an individual example call the relevant function directly.
#' @param fixed_range A logical scalar.  Only relevant to the \strong{Discrete}
#'   and \strong{Continuous} menus.  If \code{TRUE} then in the call to
#'   \code{\link{discrete}} or \code{\link{continuous}} the argument
#'   \code{var_support} (\code{discrete}) or \code{var_range}
#'   (\code{continuous}) is set so that the values on the horizontal axes
#'   are fixed at values that enable the movie to show the effects of changing
#'   the parameters of the distribution, at least locally to the default
#'   initial values for the parameters.  For greater control call
#'   \code{\link{discrete}} or \code{\link{continuous}} directly.
#' @param hscale,vscale Numeric scalars.  Scaling parameters for the size
#'   of the plot when \code{panel_plot = TRUE}. The default values are 1.4 on
#'   Unix platforms and 2 on Windows platforms.
#' @examples
#' movies()
#' @seealso \code{\link{discrete}}, \code{\link{continuous}},
#'   \code{\link{clt}}, \code{\link{ett}}, \code{\link{correlation}},
#'   \code{\link{lev_inf}}, \code{\link{wws}}, \code{\link{shypo}}.
#' @seealso \code{\link{smovie}}: general information about smovie.
#' @export
movies <- function(fixed_range = TRUE, hscale = NA, vscale = hscale) {
  if (!is.tclObj(tcltk::tclRequire("BWidget"))) {
    message("Package BWidget was not found.")
    message("Please see the smovie README file for information.")
    return()
  }
  panel.launch <- function(menu.panel) {
    if (menu.panel$demo == "binomial") {
      discrete(distn = "binomial", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "geometric") {
      if (fixed_range) {
        discrete(distn = "geometric", var_support = 0:30, hscale = hscale,
                 vscale = vscale)
      } else {
        discrete(distn = "geometric", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "hypergeometric") {
      discrete(distn = "hypergeometric", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "negative binomial") {
      if (fixed_range) {
        discrete(distn = "negative binomial", var_support = 0:100,
                 hscale = hscale, vscale = vscale)
      } else {
        discrete(distn = "negative binomial", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "Poisson") {
      if (fixed_range) {
        discrete(distn = "poisson", var_support = 0:20, hscale = hscale,
                 vscale = vscale)
      } else {
        discrete(distn = "poisson", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "beta") {
      continuous(distn = "beta", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Cauchy") {
      if (fixed_range) {
        continuous(distn = "cauchy", var_range = c(-20, 20), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "cauchy", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "chi-squared") {
      if (fixed_range) {
        continuous(distn = "chi-squared", var_range = c(0, 15),
                   hscale = hscale, vscale = vscale)
      } else {
        continuous(distn = "chi-squared", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "exponential") {
      if (fixed_range) {
        continuous(distn = "exponential", var_range = c(0, 10),
                   hscale = hscale, vscale = vscale)
      } else {
        continuous(distn = "exponential", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "F") {
      if (fixed_range) {
        continuous(distn = "f", var_range = c(0, 10), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "f", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "gamma") {
      if (fixed_range) {
        continuous(distn = "gamma", var_range = c(0, 20), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "gamma", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "GEV") {
      if (fixed_range) {
        continuous(distn = "gev", var_range = c(-5, 15), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "gev", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "GP") {
      if (fixed_range) {
        continuous(distn = "gp", var_range = c(-3, 15), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "gp", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "lognormal") {
      if (fixed_range) {
        continuous(distn = "lognormal", var_range = c(0, 15), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "lognormal", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "normal") {
      if (fixed_range) {
        continuous(distn = "normal", var_range = c(-8, 8), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "normal", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "Student t") {
      if (fixed_range) {
        continuous(distn = "t", var_range = c(-10, 10), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "t", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "uniform") {
      if (fixed_range) {
        continuous(distn = "uniform", var_range = c(-2, 3), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "uniform", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "Weibull") {
      if (fixed_range) {
        continuous(distn = "weibull", var_range = c(0, 10), hscale = hscale,
                   vscale = vscale)
      } else {
        continuous(distn = "weibull", hscale = hscale, vscale = vscale)
      }
    }
    else if (menu.panel$demo == "Pearson correlation coefficient") {
      correlation(n = 10, hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Mean") {
      clt(distn = "exponential", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Maximum") {
      ett(distn = "exponential", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Leverage and influence") {
      lev_inf(hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Wald, Wilks and Score tests") {
      wws(theta0 = 0.8, hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Testing simple hypotheses") {
      shypo(mu0 = 0, eff = 5, n = 1, hscale = hscale, vscale = vscale)
    }
    # ETT
    else if (menu.panel$demo == "beta ") {
      ett(distn = "beta", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Cauchy ") {
      ett(distn = "cauchy", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "chi-squared ") {
      ett(distn = "chi-squared", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "exponential ") {
      ett(distn = "exponential", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "F ") {
      ett(distn = "f", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "gamma ") {
      ett(distn = "gamma", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "generalized Pareto ") {
      ett(distn = "gp", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "log-normal ") {
      ett(distn = "log-normal", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "negated GEV ") {
      ett(distn = "ngev", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "normal ") {
      ett(distn = "normal", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Student t ") {
      ett(distn = "t", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "uniform ") {
      ett(distn = "uniform", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == "Weibull ") {
      ett(distn = "weibull", hscale = hscale, vscale = vscale)
    }
    # CLT
    else if (menu.panel$demo == " beta") {
      clt(distn = "beta", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " binomial") {
      clt(distn = "binomial", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " chi-squared") {
      clt(distn = "chi-squared", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " exponential") {
      clt(distn = "exponential", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " F") {
      clt(distn = "f", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " gamma") {
      clt(distn = "gamma", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " generalized Pareto") {
      clt(distn = "gp", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " geometric") {
      clt(distn = "geometric", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " hypergeometric") {
      clt(distn = "hypergeometric", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " GEV") {
      clt(distn = "gev", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " log-normal") {
      clt(distn = "log-normal", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " negative binomial") {
      clt(distn = "binomial", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " normal") {
      clt(distn = "normal", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " poisson") {
      clt(distn = "poisson", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " Student t") {
      clt(distn = "t", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " uniform") {
      clt(distn = "uniform", hscale = hscale, vscale = vscale)
    }
    else if (menu.panel$demo == " Weibull") {
      clt(distn = "weibull", hscale = hscale, vscale = vscale)
    }
    return(menu.panel)
  }
  menu.panel <- rpanel::rp.control("Movies", homer = FALSE,
                                   number.list = list(),
                                   ss = list(), trans = list(), theta = list())
  menu.list  <-  list(list("Discrete",
                           "binomial",
                           "geometric",
                           "hypergeometric",
                           "negative binomial",
                           "Poisson"
  ),
  list("Continuous",
       "beta",
       "Cauchy",
       "chi-squared",
       "exponential",
       "F",
       "gamma",
       "GEV",
       "GP",
       "lognormal",
       "normal",
       "Student t",
       "uniform",
       "Weibull"
  ),
  list("Sampling distributions",
       "Pearson correlation coefficient",
       "Mean",
       "Maximum"
  ),
  list("CLT",
       " beta",
       " binomial",
       " Cauchy",
       " chi-squared",
       " exponential",
       " F",
       " gamma",
       " generalized Pareto",
       " geometric",
       " hypergeometric",
       " log-normal",
       " GEV",
       " negative binomial",
       " normal",
       " poisson",
       " Student t",
       " uniform",
       " Weibull"
  ),
  list("Regression",
       "Leverage and influence"
  ),
  list("Hypothesis testing",
       "Wald, Wilks and Score tests",
       "Testing simple hypotheses"
       ),
  list("Extremal types",
       "beta ",
       "Cauchy ",
       "chi-squared ",
       "exponential ",
       "F ",
       "gamma ",
       "generalized Pareto ",
       "log-normal ",
       "negated GEV ",
       "normal ",
       "Student t ",
       "uniform ",
       "Weibull "
       )
  )
  demo <- NULL
  rpanel::rp.menu(menu.panel, variable = demo, labels = menu.list,
                  action = panel.launch)
  image.file <- file.path(system.file(package = "rpanel"), "images",
                          "cartoons.gif")
  rpanel::rp.image(menu.panel, image.file)
  return(invisible())
}
