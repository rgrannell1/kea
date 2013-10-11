\name{xConcat}
\alias{xConcat}
\title{xConcat}
\usage{
  xConcat(...)
}
\arguments{
  \item{...}{several collections.}
}
\value{
  a list.
}
\description{
  Concatenate several collections into one collection.
}
\section{Corner Cases}{
  empty collections act as a unit for concatenation;
  concatenating the empty list to another list returns the
  second list, without modification.
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

