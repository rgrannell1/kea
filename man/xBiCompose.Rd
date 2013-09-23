\name{xBiCompose}
\alias{xBiCompose}
\title{xBiCompose}
\usage{
  xBiCompose(fn1, fn2, fn3)
}
\arguments{
  \item{fn1}{a binary function.}

  \item{fn2}{a unary function.}

  \item{fn3}{a unary function.}
}
\value{
  returns a unary function of x.
}
\description{
  Compose a binary function with two other functions.
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

