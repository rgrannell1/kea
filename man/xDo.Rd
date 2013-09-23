\name{xDo}
\alias{xDo}
\title{xDo}
\usage{
  xDo(fn, coll)
}
\arguments{
  \item{fn}{a unary function, usually side-effectful.}

  \item{coll}{a collection}
}
\value{
  a list.
}
\description{
  Map (a possibly side-effectful) function over a
  collection and discard the results.
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

