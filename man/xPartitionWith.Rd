\name{xPartitionWith}
\alias{xPartitionWith}
\title{xPartitionWith}
\usage{
  xPartitionWith(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}
}
\value{
  Returns two lists; a list of elements in a collection for
  which a predicate returns true, and a list of elements in
  a collection for which a predicate returns false or na.
}
\description{
  Divide elements in a collection into two collections
  based on a predicate function.
}
\section{Corner Cases}{
  if \code{coll} is empty the empty list is returned. If
  all the elements return only true/only false, then one of
  two sublists will be the empty list.
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

