\name{xPluck}
\alias{xPluck}
\title{xPluck}
\usage{
  xPluck(str, coll)
}
\arguments{
  \item{str}{a string.}

  \item{coll}{a list or pairlist of lists or pairlists.}
}
\value{
  a list.
}
\description{
  Map over a collection of lists or pairlists, selecting
  fields in each element by name.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero. If
  str is length-zero then the empty string "" is used to
  match key-names.
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

