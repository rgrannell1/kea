\name{xUnionLift}
\alias{xUnionLift}
\title{xUnionLift}
\usage{
  xUnionLift(fn1, fn2)
}
\arguments{
  \item{fn1}{a unary function that returns a collection.}

  \item{fn2}{a unary function that returns a collection.}
}
\value{
  a unary function.
}
\description{
  Compose two function with the set union.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
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

