\name{xOrLift}
\alias{xOrLift}
\title{xOrLift}
\usage{
  xOrLift(pred1, pred2)
}
\arguments{
  \item{pred1}{a predicate.}

  \item{pred2}{a predicate.}
}
\value{
  a logical value.
}
\description{
  Return a function that tests if either of two functions
  are true for a particular value.
}
\section{Glossary}{
  \bold{Function}: a closure or primitive function.  A
  symbol or string refering to such a function may also be
  used.

  \bold{Predicate}: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  \bold{Collection}: A list, pairlist or a typed vector.

  \bold{Arity}: The number of arguments a function can
  accept.

  \bold{Nullary}: a function with arity zero. \bold{Unary}:
  a function with arity one. \bold{Binary}: a function with
  arity two. \bold{Trinary}: a function with arity three.

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.
}
\examples{
inst/examples/blank.R
}
\seealso{
  Other function_lifting: \code{\link{xAndLift}},
  \code{\link{xByLift}}, \code{\link{xEqualLift}},
  \code{\link{xInterLift}}, \code{\link{xMinusLift}},
  \code{\link{xModLift}}, \code{\link{xOverLift}},
  \code{\link{xPlusLift}}, \code{\link{xUnionLift}}

  Other higher_order_function: \code{\link{xAndLift}},
  \code{\link{xByLift}}, \code{\link{xDifferLift}},
  \code{\link{xEqualLift}}, \code{\link{xInterLift}},
  \code{\link{xMinusLift}}, \code{\link{xModLift}},
  \code{\link{xOverLift}}, \code{\link{xPlusLift}},
  \code{\link{xUnionLift}}
}

