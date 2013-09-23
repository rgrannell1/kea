\name{xNotNa}
\alias{xNotNa}
\title{xNotNa}
\usage{
  xNotNa(coll)
}
\arguments{
  \item{coll}{a collection.}
}
\value{
  a vector of boolean values.
}
\description{
  Is an element of a collection not na?
}
\section{Corner Cases}{
  returns logical(0) if \code{coll} is length-zero.
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

