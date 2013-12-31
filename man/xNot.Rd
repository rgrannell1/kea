\name{xNot}
\alias{xNot}
\title{xNot}
\usage{
xNot(pred)
}
\arguments{
  \item{pred}{a predicate of any arity.}
}
\value{
a predicate function of val.
}
\description{
Return the logical negation of a function.
}
\section{Uses}{
  \code{xNot} is useful when used alongside higher-order
  functions that takes a logical function as an input. For
  example, given a function to test if a value is an
  integer and the select function, it would be possible to
  select every value that is not an integer using
  \code{xNot}.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}};
\code{\link{xWait}}
}

