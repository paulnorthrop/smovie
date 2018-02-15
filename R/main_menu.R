smovie_menu <- function(hscale = 1) {

  panel.launch <- function(menu.panel) {
    if (menu.panel$demo =="Binomial p.m.f") {
      binom_pmf()
    }
    else if (menu.panel$demo == "Wald, Wilks and Score tests") {
      wws(theta0 = 0.8)
    }
    else if (menu.panel$demo == "Testing simple hypotheses") {
      shypo(mu0 = 0, eff = 5, n = 1)
    }
    menu.panel
  }

  menu.panel <- rp.control("Cartoons", homer = FALSE, number.list = list(),
                           ss = list(), trans = list(), theta = list())
  menu.list  <-  list(list("Distributions",
                           "Binomial p.m.f."
  ),
  list("Hypothesis testing",
       "Wald, Wilks and Score tests",
       "Testing simple hypotheses"
       )
  )

  if (!require(sm)) menu.list <- menu.list[-4]
  rp.menu(menu.panel, demo, menu.list, action = panel.launch)
  image.file <- file.path(system.file(package = "rpanel"), "images",
                          "cartoons.gif")
  rp.image(menu.panel, image.file)

  invisible()
}
