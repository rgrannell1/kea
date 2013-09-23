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

