\name{xSegment}
\alias{xSegment}
\title{xSegment}
\usage{
  xSegment(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
  a list of n-element lsits.
}
\description{
  Divide a collection into segments of fixed length.
}
\section{Corner Cases}{
  the final list in the return value will have less than
  \code{num} elements if \code{length(coll)} is not evenly
  divisible by \code{num}.  if \code{coll} is length-zero,
  the empty list is returned.
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

