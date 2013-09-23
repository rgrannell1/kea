\name{xRecurMap}
\alias{xRecurMap}
\title{xRecurMap}
\usage{
  xRecurMap(fn, coll)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a list or pairlist.}
}
\value{
  a list or pairlist.
}
\description{
  Recursively map a function into a nested collection,
  preserving its structure.
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

