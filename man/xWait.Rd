\name{xWait}
\alias{xWait}
\title{xWait}
\usage{
  xWait(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{num}{a nonnegative whole number.}
}
\value{
  a function with the same parameters as \code{fn}.
}
\description{
  Delay the evalution of a function after invokation for a
  set amount of time.
}
\section{Corner Cases}{
  if \code{num} is zero then \code{fn} is returned
  untouched.
}
\seealso{
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xFlip}},
  \code{\link{xFormals}}, \code{\link{xIterate}},
  \code{\link{xJuxtapose}}, \code{\link{xLift}},
  \code{\link{xLimit}}, \code{\link{xNot}},
  \code{\link{xParams}}, \code{\link{xUnfold}}
}

