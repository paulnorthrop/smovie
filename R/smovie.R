#' smovie: some movies to illustrate concepts in statistics
#'
#' These movies are animations used to illustrate key statistical ideas.
#' They are produced using the \code{\link[rpanel]{rpanel-package}}.
#'
#' @details When one of these functions is called R opens up a small
#' \emph{parameter window} containing clickable buttons that can be
#' used to change parameters underlying the plot. For the effects of
#' these buttons see the documentation of the individual functions.
#'
#' See \code{vignette("smovie-vignette", package = "smovie")} for an overview
#' of the package and the \link[=movies]{user-friendly menu panel}.
#'
#' There are movies on the following topics.
#'
#' @references Bowman, A., Crawford, E., Alexander, G. and Bowman, R. W.
#'  (2007). rpanel: Simple Interactive Controls for R Functions Using the
#'  tcltk Package.  \emph{Journal of Statistical Software}, \strong{17(9)},
#'  1-18. \url{http://www.jstatsoft.org/v17/i09/}.
#'
#' @section {Probability distributions}:
#' \itemize{
#'   \item {\link[=discrete]{Discrete distributions}}
#'   \item {\link[=continuous]{Continuous distributions}}
#' }
#'
#' @section {Sampling distributions}:
#' \itemize{
#'   \item {\link[=clt]{Central Limit Theorem: sampling distribution
#'     of a sample mean}}
#'   \item {\link[=ett]{Extremal Types Theorem: sampling distribution
#'     of a sample maximum}}
#'   \item {\link[=correlation]{Pearson product moment correlation
#'     coefficient}}
#' }
#'
#' @section {Regression}:
#' \itemize{
#'   \item {\link[=lev_inf]{Leverage and influence in simple linear
#'     regression}}
#' }
#'
#' @section {Hypothesis testing}:
#' \itemize{
#'   \item {\link[=wws]{Wald, Wilks and Score tests}}
#'   \item {\link[=shypo]{Testing simple hypotheses}}
#' }
#' @docType package
#' @name smovie
#' @import methods
#' @importFrom rpanel rp.control
NULL
