\name{xCompose}
\alias{xCompose}
\title{xCompose}
\usage{
  xCompose(fn1, fn2)
}
\arguments{
  \item{fn1}{a unary function.}

  \item{fn2}{a unary function.}
}
\value{
  A function with the same parameters as \code{fn2}.
}
\description{
  Compose two functions.
}
\details{
  Unlike some other functions that return functions in
  Arrow, \code{xCompose} preserves the parameters of one of
  its input functions (the right function). This is not
  always possible for some other functions, but it is
  included in \code{xCompose} as the function is ubiquitous
  and should be as powerful as possible, even at the risk
  of making Arrow less consistent.
}
\seealso{
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xConst}},
  \code{\link{xFlip}}, \code{\link{xFormals}},
  \code{\link{xIterate}}, \code{\link{xJuxtapose}},
  \code{\link{xLift}}, \code{\link{xLimit}},
  \code{\link{xNot}}, \code{\link{xParams}},
  \code{\link{xUnfold}}, \code{\link{xWait}}
}

