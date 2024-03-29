% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smovie-package.R
\docType{package}
\name{smovie-package}
\alias{smovie}
\alias{smovie-package}
\title{smovie: some movies to illustrate concepts in statistics}
\description{
These movies are animations used to illustrate key statistical ideas.
They are produced using the \code{\link[rpanel:rpanel.package]{rpanel}}
package.
}
\details{
When one of these functions is called R opens up a small
\emph{parameter window} containing clickable buttons that can be
used to change parameters underlying the plot. For the effects of
these buttons see the documentation of the individual functions.

See \code{vignette("smovie-vignette", package = "smovie")} for an overview
of the package and the \link[=movies]{user-friendly menu panel}.

There are movies on the following topics.
}
\section{Probability distributions}{

\itemize{
  \item \link[=discrete]{Discrete distributions}
  \item \link[=continuous]{Continuous distributions}
}
}

\section{Sampling distributions}{

\itemize{
  \item \link[=clt]{Central Limit Theorem: sampling distribution
    of a sample mean}
  \item \link[=cltq]{Central Limit Theorem for sample quantiles: sampling
    distribution of the 100p\% sample quantile}
  \item \link[=mean_vs_median]{Sampling distributions of the mean
    and median}
  \item \link[=ett]{Extremal Types Theorem: sampling distribution
    of a sample maximum}
  \item \link[=correlation]{Correlation coefficient}
}
}

\section{Regression}{

\itemize{
  \item \link[=lev_inf]{Leverage and influence in simple linear
    regression}
}
}

\section{Hypothesis testing}{

\itemize{
  \item \link[=wws]{Wald, Wilks and Score tests}
  \item \link[=shypo]{Testing simple hypotheses}
}
}

\references{
Bowman, A., Crawford, E., Alexander, G. and Bowman, R. W.
 (2007). rpanel: Simple Interactive Controls for R Functions Using the
 tcltk Package.  \emph{Journal of Statistical Software}, \strong{17(9)},
 1-18. \doi{10.18637/jss.v017.i09}.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://paulnorthrop.github.io/smovie/}
  \item \url{https://github.com/paulnorthrop/smovie/}
  \item Report bugs at \url{https://github.com/paulnorthrop/smovie/issues}
}

}
\author{
\strong{Maintainer}: Paul J. Northrop \email{p.northrop@ucl.ac.uk} [copyright holder]

}
