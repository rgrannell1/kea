\name{xIterate}
\alias{xIterate}
\title{Iteratively apply a function to a value.}
\usage{
  xIterate(fn, init)
}
\arguments{
  \item{fn}{a function.}

  \item{init}{an arbitrary value.}
}
\value{
  the result of successively applying \code{f} to
  \code{init}.
}
\description{
  Iteratively apply a function to a value.
}
\section{Corner Cases}{
  length-zero values of \code{init} are handled normally,
  since \code{init} is an arbitrary value. Potentially
  non-terminating.
}
\seealso{
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xFlip}},
  \code{\link{xFormals}}, \code{\link{xJuxtapose}},
  \code{\link{xLift}}, \code{\link{xLimit}},
  \code{\link{xNot}}, \code{\link{xParams}},
  \code{\link{xUnfold}}, \code{\link{xWait}}
}

