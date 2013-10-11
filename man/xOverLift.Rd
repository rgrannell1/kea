\name{xOverLift}
\alias{xOverLift}
\title{xOverLift}
\usage{
  xOverLift(fn1, fn2)
}
\arguments{
  \item{fn1}{a unary function that returns a number.}

  \item{fn2}{a unary function that returns a number.}
}
\value{
  a number.
}
\description{
  Compose two functions with division.
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
  \code{\link{xModLift}}, \code{\link{xOrLift}},
  \code{\link{xPlusLift}}, \code{\link{xUnionLift}}

  Other higher_order_function: \code{\link{xAndLift}},
  \code{\link{xByLift}}, \code{\link{xDifferLift}},
  \code{\link{xEqualLift}}, \code{\link{xInterLift}},
  \code{\link{xMinusLift}}, \code{\link{xModLift}},
  \code{\link{xOrLift}}, \code{\link{xPlusLift}},
  \code{\link{xUnionLift}}
}

