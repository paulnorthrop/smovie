#' Main menu for smovie movies
#'
#' Uses the template \code{\link[rpanel]{rp.cartoons}} function to produce
#' a menu panel from which any of the movies in
#' \code{\link[smovie]{smovie}} package can be launched.
#' @param fixed_range A logical scalar.  Only relevant to the \strong{Discrete}
#'   and \strong{Continuous} menus.  If \code{TRUE} then in the call to
#'   \code{\link{discrete}} or \code{\link{continuous}} the argument
#'   \code{var_support} (\code{dicrete}) or \code{var_range}
#'   (\code{continuous}) is set so that the values on the horizontal axes
#'   are fixed at values that enable the movie to show the effects of changing
#'   the parameters of the distribution, at least locally to the default
#'   initial values for the parameters.  For greater control call
#'   \code{\link{discrete}} or \code{\link{continuous}} directly.
#' @param hscale A numeric scalar.  A scaling parameter for the size of the
#'   plot, which will be passed to all relevant menu items.
#' @export
movies <- function(fixed_range = TRUE, hscale = 1) {
  panel.launch <- function(menu.panel) {
    if (menu.panel$demo == "binomial") {
      discrete(distn = "binomial")
    }
    else if (menu.panel$demo == "geometric") {
      if (fixed_range) {
        discrete(distn = "geometric", var_support = 0:30)
      } else {
        discrete(distn = "geometric")
      }
    }
    else if (menu.panel$demo == "hypergeometric") {
      discrete(distn = "hypergeometric")
    }
    else if (menu.panel$demo == "negative binomial") {
      if (fixed_range) {
        discrete(distn = "negative binomial", var_support = 0:100)
      } else {
        discrete(distn = "negative binomial")
      }
    }
    else if (menu.panel$demo == "Poisson") {
      if (fixed_range) {
        discrete(distn = "poisson", var_support = 0:20)
      } else {
        discrete(distn = "poisson")
      }
    }
    else if (menu.panel$demo == "beta") {
      continuous(distn = "beta")
    }
    else if (menu.panel$demo == "Cauchy") {
      continuous(distn = "cauchy")
    }
    else if (menu.panel$demo == "chi-squared") {
      continuous(distn = "chi-squared")
    }
    else if (menu.panel$demo == "exponential") {
      continuous(distn = "exponential")
    }
    else if (menu.panel$demo == "F") {
      continuous(distn = "f")
    }
    else if (menu.panel$demo == "gamma") {
      continuous(distn = "gamma")
    }
    else if (menu.panel$demo == "GEV") {
      continuous(distn = "gev")
    }
    else if (menu.panel$demo == "GP") {
      continuous(distn = "gp")
    }
    else if (menu.panel$demo == "lognormal") {
      continuous(distn = "lognormal")
    }
    else if (menu.panel$demo == "normal") {
      continuous(distn = "normal")
    }
    else if (menu.panel$demo == "Student t") {
      continuous(distn = "t")
    }
    else if (menu.panel$demo == "uniform") {
      continuous(distn = "uniform")
    }
    else if (menu.panel$demo == "Weibull") {
      continuous(distn = "weibull")
    }
    else if (menu.panel$demo == "Pearson correlation coefficient") {
      correlation(n = 10)
    }
    else if (menu.panel$demo == "Maximum") {
      ett(distn = "exponential")
    }
    else if (menu.panel$demo == "Outliers: leverage and influence") {
      lev_inf()
    }
    else if (menu.panel$demo == "Wald, Wilks and Score tests") {
      wws(theta0 = 0.8)
    }
    else if (menu.panel$demo == "Testing simple hypotheses") {
      shypo(mu0 = 0, eff = 5, n = 1)
    }
    else if (menu.panel$demo == "beta") {
      ett(distn = "beta")
    }
    else if (menu.panel$demo == "Cauchy") {
      ett(distn = "cauchy")
    }
    else if (menu.panel$demo == "chi-squared") {
      ett(distn = "chi-squared")
    }
    else if (menu.panel$demo == "exponential") {
      ett(distn = "exponential")
    }
    else if (menu.panel$demo == "F") {
      ett(distn = "f")
    }
    else if (menu.panel$demo == "gamma") {
      ett(distn = "gamma")
    }
    else if (menu.panel$demo == "generalized Pareto") {
      ett(distn = "gp")
    }
    else if (menu.panel$demo == "log-normal") {
      ett(distn = "log-normal")
    }
    else if (menu.panel$demo == "negated GEV") {
      ett(distn = "ngev")
    }
    else if (menu.panel$demo == "normal") {
      ett(distn = "normal")
    }
    else if (menu.panel$demo == "Student t") {
      ett(distn = "t")
    }
    else if (menu.panel$demo == "uniform") {
      ett(distn = "uniform")
    }
    else if (menu.panel$demo == "Weibull") {
      ett(distn = "weibull")
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
       "Maximum"
  ),
  list("Regression",
       "Outliers: leverage and influence"
  ),
  list("Hypothesis testing",
       "Wald, Wilks and Score tests",
       "Testing simple hypotheses"
       ),
  list("Extremal types",
       "beta",
       "Cauchy",
       "chi-squared",
       "exponential",
       "F",
       "gamma",
       "generalized Pareto",
       "log-normal",
       "negated GEV",
       "normal",
       "Student t",
       "uniform",
       "Weibull"
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
