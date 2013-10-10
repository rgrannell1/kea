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
  \bold{Function}: a closure or primitive function.  A
  symbol or string refering to such a function may also be
  used.

  \bold{Predicate}: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  \bold{Collection}: A list, pairlist or a typed vector.

  \bold{Arity}: The number of arguments a function can
  accept.  Unary (1), Binary (2), Trinary (3), Quaternary
  (4), ...., Variadic (Inf).

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.
}
\examples{
inst/examples/blank.R
}

