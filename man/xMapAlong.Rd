\name{xMapAlong}
\alias{xMapAlong}
\title{xMapAlong}
\usage{
  xMapAlong(fn, coll)
}
\arguments{
  \item{fn}{a binary function.}

  \item{coll}{a collection.}
}
\value{
  a list.
}
\description{
  Apply a binary function to each element of a collection.
  and its indices.
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

