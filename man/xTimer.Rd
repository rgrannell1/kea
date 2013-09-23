\name{xTimer}
\alias{xTimer}
\title{xTimer}
\usage{
  xTimer(num)
}
\arguments{
  \item{num}{a positive number.}
}
\value{
  a variadic function that ignores its arguments.
}
\description{
  Create a function that returns true for a certain amount
  of time after its creation.
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

