#' Main menu for smovie movies
#'
#' Uses the template \code{\link[rpanel]{rp.cartoons}} function to produce
#' a menu panel from which any of the movies in
#' \code{\link[smovie]{smovie}} package can be launched.
#' @param hscale Add description.
#' @export
movies <- function(hscale = 1) {
  panel.launch <- function(menu.panel) {
    if (menu.panel$demo == "binomial") {
      discrete(distn = "binomial")
    }
    else if (menu.panel$demo == "geometric") {
      discrete(distn = "geometric")
    }
    else if (menu.panel$demo == "hypergeometric") {
      discrete(distn = "hypergeometric")
    }
    else if (menu.panel$demo == "negative binomial") {
      discrete(distn = "negative binomial")
    }
    else if (menu.panel$demo == "poisson") {
      discrete(distn = "poisson")
    }
    else if (menu.panel$demo == "beta") {
      continuous(distn = "beta")
    }
    else if (menu.panel$demo == "cauchy") {
      continuous(distn = "cauchy")
    }
    else if (menu.panel$demo == "chi-squared") {
      continuous(distn = "chi-squared")
    }
    else if (menu.panel$demo == "exponential") {
      continuous(distn = "exponential")
    }
    else if (menu.panel$demo == "normal") {
      continuous(distn = "normal")
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
                           "poisson"
  ),
  list("Continuous",
       "beta",
       "cauchy",
       "chi-squared",
       "exponential",
       "normal"
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
