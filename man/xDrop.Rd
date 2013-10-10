\name{xDrop}
\alias{xDrop}
\title{xDrop}
\usage{
  xDrop(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection.}
}
\value{
  a list.
}
\description{
  Take several elements from the front of a collection.
}
\section{Corner Cases}{
  If \code{coll} is empty the empty list is returned.
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

