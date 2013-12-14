\name{xConst}
\alias{xConst}
\title{xConst}
\usage{
xConst(val)
}
\arguments{
  \item{val}{an arbitrary value.}
}
\value{
a variadic function.
}
\description{
Create a function that returns a particular value.
}
\section{Uses}{
  \code{xConst} is useful when used alongside the lift
  family of functions. For example, the \code{xModLift}
  function could be partially applied with \code{xConst(6)}
  as its left argument to create a \code{xModLift6}
  function, that modulo-6s the output of its underlying
  function. \code{xConst} is also useful for creating
  function analogues to TRUE, FALSE and NA for use with
  arrow functions that take a predicate.
}
\seealso{
Other higher_order_functions: \code{\link{xArity}};
\code{\link{xAsUnary}}; \code{\link{xCompose}};
\code{\link{xFlip}}; \code{\link{xFormals}};
\code{\link{xIterate}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xStopWatch}}; \code{\link{xUnfold}};
\code{\link{xWait}}
}

