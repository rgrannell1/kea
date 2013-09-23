\name{xSelect}
\alias{xSelect}
\title{xSelect}
\usage{
  xSelect(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}
}
\value{
  a list.
}
\description{
  Include all elements from a collection matching a
  predicate.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero, or
  no match is found.
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

