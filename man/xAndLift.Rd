\name{xAndLift}
\alias{xAndLift}
\title{xAndLift}
\usage{
  xAndLift(pred1, pred2)
}
\arguments{
  \item{pred1}{an n-ary predicate.}

  \item{pred2}{an n-ary predicate.}
}
\value{
  a n-ary predicate.
}
\description{
  Return a function that then tests if a pair of functions
  are true for its input.
}
\section{Glossary}{
  function: a closure or primitive function.  A symbol or
  string refering to such a function may also be used.

  predicate: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  collection: A list, pairlist or vector of any type.

  arity: The number of arguments a function can accept.
  Variadic functions have an arity of +Infinity,

  number: a double, integer or complex number, unless
  stated otherwise.
}
\examples{
inst/examples/blank.R
}

