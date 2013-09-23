\name{xMap}
\alias{xMap}
\title{xMap}
\usage{
  xMap(fn, coll)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}
}
\value{
  a list.
}
\description{
  Apply a function to each element of a collection.
}
\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
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

