\name{xMapAlong}
\alias{xMapAlong}
\title{xMapAlong}
\usage{
  xMapAlong(fn, coll)
}
\arguments{
  \item{fn}{a binary function.}

  \item{coll}{a collection.}
}
\value{
  a list.
}
\description{
  Apply a binary function to each element of a collection.
  and its indices.
}
\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
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
  accept.

  \bold{Nullary}: a function with arity zero. \bold{Unary}:
  a function with arity one. \bold{Binary}: a function with
  arity two. \bold{Trinary}: a function with arity three.

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.
}
\examples{
inst/examples/blank.R
}

