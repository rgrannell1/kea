\name{xIsVariadic}
\alias{xIsVariadic}
\title{xIsVariadic}
\usage{
  xIsVariadic(fn)
}
\arguments{
  \item{fn}{an arbitrary function or primitive function.}
}
\value{
  a boolean value.
}
\description{
  Is a function variadic?
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

