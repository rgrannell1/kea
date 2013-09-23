\name{xPack}
\alias{xPack}
\title{xPack}
\usage{
  xPack(coll)
}
\arguments{
  \item{coll}{a collection.}
}
\value{
  returns a list of elements in \code{coll}, with all
  length-zero values removed.
}
\description{
  Remove all length-zero values from a collection.
}
\section{Corner Cases}{
  Returns the emty list if \code{coll} is length-zero, or
  all elements in \code{coll} are length-zero.
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

