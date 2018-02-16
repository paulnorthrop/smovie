#' Main menu for smovie movies
#'
#' Uses the template \code{\link[rpanel]{rp.cartoons}} function to produce
#' a menu panel from which any of the movies in
#' \code{\link[smovie]{smovie-package}} can be launched.
#' @param hscale Add description.
#' @export
movies <- function(hscale = 1) {
  panel.launch <- function(menu.panel) {
    if (menu.panel$demo =="Binomial pmf") {
      binom_pmf()
    }
    else if (menu.panel$demo == "Correlation coefficient") {
      corr_sim(n = 10)
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
    else if (menu.panel$demo == "ETT: beta") {
      ett(distn = "beta")
    }
    else if (menu.panel$demo == "ETT: Cauchy") {
      ett(distn = "cauchy")
    }
    else if (menu.panel$demo == "ETT: chi-squared") {
      ett(distn = "chi-squared")
    }
    else if (menu.panel$demo == "ETT: exponential") {
      ett(distn = "exponential")
    }
    else if (menu.panel$demo == "ETT: F") {
      ett(distn = "f")
    }
    else if (menu.panel$demo == "ETT: gamma") {
      ett(distn = "gamma")
    }
    else if (menu.panel$demo == "ETT: generalized Pareto") {
      ett(distn = "gp")
    }
    else if (menu.panel$demo == "ETT: log-normal") {
      ett(distn = "log-normal")
    }
    else if (menu.panel$demo == "ETT: negated GEV") {
      ett(distn = "ngev")
    }
    else if (menu.panel$demo == "ETT: normal") {
      ett(distn = "normal")
    }
    else if (menu.panel$demo == "ETT: Student t") {
      ett(distn = "t")
    }
    else if (menu.panel$demo == "ETT: uniform") {
      ett(distn = "uniform")
    }
    else if (menu.panel$demo == "ETT: Weibull") {
      ett(distn = "weibull")
    }
    return(menu.panel)
  }

  menu.panel <- rpanel::rp.control("Movies", homer = FALSE,
                                   number.list = list(),
                                   ss = list(), trans = list(), theta = list())
  menu.list  <-  list(list("Distributions",
                           "Binomial pmf"
  ),
  list("Sampling distributions",
       "Correlation coefficient",
       "Maximum"
  ),
  list("Regression",
       "Outliers: leverage and influence"
  ),
  list("Hypothesis testing",
       "Wald, Wilks and Score tests",
       "Testing simple hypotheses"
       ),
  list("Extreme value theory",
       "ETT: beta",
       "ETT: Cauchy",
       "ETT: chi-squared",
       "ETT: exponential",
       "ETT: F",
       "ETT: gamma",
       "ETT: generalized Pareto",
       "ETT: log-normal",
       "ETT: negated GEV",
       "ETT: normal",
       "ETT: Student t",
       "ETT: uniform",
       "ETT: Weibull"
       )
  )

  rpanel::rp.menu(menu.panel, demo, menu.list, action = panel.launch)
  image.file <- file.path(system.file(package = "rpanel"), "images",
                          "cartoons.gif")
  rpanel::rp.image(menu.panel, image.file)
  return(invisible())
}
