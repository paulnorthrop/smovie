#' smovie: some movies to illustrate concepts in statistics
#'
#' These movies are animations used to illustrate key statistical ideas.
#' They are produced using the package \code{\link[rpanel]{rpanel}}.
#'
#' @details When one of these functions is called R opens up a small
#' \emph{parameter window} containing clickable buttons that can be
#' used to change parameters underlying the plot. For the effects of
#' these buttons see the documentation of the individual functions.
#'
#' When a button (produced using \code{\link[rpanel]{rp.button}} or
#' \code{\link[rpanel]{rp.doublebutton}}) ...
#'
#' The parameter window does not close automatically after the movie:
#' the user needs to close it manually.
#' @references Bowman, A., Crawford, E., Alexander, G. and Bowman, R. W.
#'  (2007). rpanel: Simple Interactive Controls for R Functions Using the
#'  tcltk Package.  \emph{Journal of Statistical Software}, \strong{17(9)},
#'  1-18. \url{http://www.jstatsoft.org/v17/i09/}.
#'
#' @section {Probability distributions}:
#' \itemize{
#'   \item {\link[=binom_pmf]{Binomial p.m.f.}}
#' }
#'
#' @section {Sampling distributions}:
#' \itemize{
#'   \item {\link[=clt_norm]{Central Limit Theorem: normal data}}
#'   \item {\link[=clt_exp]{Central Limit Theorem: exponential data}}
#'   \item {\link[=corr_sim]{Sampling distribution of the correlation
#'     coefficient}}
#'   \item {\link[=ett]{Extremal Types Theorem: sampling distribution
#'     of a sample maximum}}
#' }
#'
#' @section {Regression and correlation}:
#' \itemize{
#'   \item {\link[=lev_inf]{Leverage and influence in simple linear
#'     regression}}
#'   \item {\link[=corr_sim]{Sampling distribution of the correlation
#'     coefficient}}
#' }
#'
#' @section {Hypothesis testing}:
#' \itemize{
#'   \item {\link[=wws]{Wald, Wilks and Score tests}}
#'   \item {\link[=shypo]{Testing simple hypotheses}}
#' }
#'
#' @section {Extreme Value Theory}:
#' \itemize{
#'   \item {\link[=ett]{Extremal Types Theorem: sampling distribution
#'     of a sample maximum}}
#' }
#' @docType package
#' @name smovie
#' @import methods
#' @importFrom rpanel rp.control
NULL
