\name{xLimit}
\alias{xLimit}
\title{xLimit}
\usage{
  xLimit(num, fn)
}
\arguments{
  \item{num}{a positive whole number.}

  \item{fn}{an arbitrary function.}
}
\value{
  a function with the same parameters as \code{fn}.
}
\description{
  Create a function that can call its underlying function a
  limited number of times.
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

