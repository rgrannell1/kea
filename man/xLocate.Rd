\name{xLocate}
\alias{xLocate}
\title{xLocate}
\usage{
  xLocate(pred, coll)
}
\arguments{
  \item{pred}{a predicate function.}
}
\value{
  an integer.
}
\description{
  Get the position of the first element for which a
  predicate returns true.
}
\section{Corner Cases}{
  returns integer(0) if no match is found.
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

