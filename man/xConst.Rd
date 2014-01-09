\name{xConst}
\alias{xConst}
\alias{xK}
\title{xConst}
\usage{
xConst(val)

xK(val)
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
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

