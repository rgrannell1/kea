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
  Other higher_order_functions: \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xChars}},
  \code{\link{xCompose}}, \code{\link{xConst}},
  \code{\link{xFlip}}, \code{\link{xIterate}},
  \code{\link{xJuxtapose}}, \code{\link{xLimit}},
  \code{\link{xParams}}, \code{\link{xUnfold}},
  \code{\link{xWait}}
}

