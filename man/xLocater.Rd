\name{xLocater}
\alias{xLocater}
\title{xLocater}
\usage{
  xLocater(pred, coll)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{coll}{a collection.}
}
\value{
  an integer.
}
\description{
  Get the position of the last element for which a
  predicate returns true.
}
\section{Corner Cases}{
  returns integer(0) if no match is found.
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

