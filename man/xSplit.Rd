\name{xSplit}
\alias{xSplit}
\title{xSplit}
\usage{
  xSplit(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
  a list of two lists; the first list containing the first
  \code{num} elements of \code{coll}, and the second list
  containing the remaining elements \code{coll}.
}
\description{
  Split a collection into elements before and upto an
  index, and after that index.
}
\section{Corner Cases}{
  If \code{num} is zero then the first list in the returned
  value is empty.  Likewise, if \code{num} is equal or
  larger than the length of \code{coll} then the second
  return list is empty. If \code{coll} is length zero both
  lists are empty.
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

