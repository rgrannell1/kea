\name{xDropWhile}
\alias{xDropWhile}
\title{xDropWhile}
\usage{
  xDropWhile(pred, coll)
}
\arguments{
  \item{pred}{a unary predicate.}

  \item{coll}{a collection.}
}
\value{
  a list.
}
\description{
  Take every element in a collection from the first time a
  predicate is false or na until the end of the collection.
}
\section{Corner Cases}{
  Returns the emty list if \code{coll} is length-zero.
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

