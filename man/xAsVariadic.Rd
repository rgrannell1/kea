\name{xAsVariadic}
\alias{xAsVariadic}
\title{xAsVariadic}
\usage{
  xAsVariadic(fn)
}
\arguments{
  \item{fn}{a unary function.}
}
\value{
  a variadic function.
}
\description{
  Return a variadic function that passes its arguments as a
  list to ints underyling function.
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

