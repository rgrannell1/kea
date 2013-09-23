\name{xByLift}
\alias{xByLift}
\title{xByLift}
\usage{
  xByLift(fn1, fn2)
}
\arguments{
  \item{fn1}{a unary function that returns a number.}

  \item{fn2}{a unary function that returns a number.}
}
\value{
  a unary function.
}
\description{
  Compose two functions with multiplication.
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

