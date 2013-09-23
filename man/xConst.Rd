\name{xConst}
\alias{xConst}
\title{xConst}
\usage{
  xConst(val)
}
\arguments{
  \item{val}{an arbitrary value.}
}
\value{
  a variadic function.
}
\description{
  Create a function that returns a particular value.
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

