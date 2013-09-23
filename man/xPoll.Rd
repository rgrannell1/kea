\name{xPoll}
\alias{xPoll}
\title{xPoll}
\usage{
  xPoll(pred, coll)
}
\arguments{
  \item{pred}{a unary predicate function.}

  \item{coll}{a collection.}
}
\value{
  a non-negative whole number.
}
\description{
  Count the number of times a function returns true when
  mapped over a collection.
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

