\name{xLimit}
\alias{xLimit}
\title{xLimit}
\usage{
  xLimit(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{num}{a positive whole number.}
}
\value{
  a function with the same parameters as \code{fn}.
}
\description{
  Create a function that can call its underlying function a
  limited number of times.
}
\seealso{
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xFlip}},
  \code{\link{xFormals}}, \code{\link{xIterate}},
  \code{\link{xJuxtapose}}, \code{\link{xLift}},
  \code{\link{xNot}}, \code{\link{xParams}},
  \code{\link{xUnfold}}, \code{\link{xWait}}
}

