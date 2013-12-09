\name{xUnfold}
\alias{xUnfold}
\title{xUnfold}
\usage{
  xUnfold(pred, fn, init)
}
\arguments{
  \item{pred}{a unary function.}

  \item{fn}{a unary function that returns a length-two
  collection.}

  \item{init}{an arbitrary value.}
}
\value{
  a list.
}
\description{
  Generate a list of values from an initial value.
}
\section{Corner Cases}{
  returns the empty list if \code{init} is length-zero.
}
\seealso{
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xFlip}},
  \code{\link{xFormals}}, \code{\link{xIterate}},
  \code{\link{xJuxtapose}}, \code{\link{xLift}},
  \code{\link{xLimit}}, \code{\link{xNot}},
  \code{\link{xParams}}, \code{\link{xWait}}
}

