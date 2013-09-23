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
  Divide elements in a collection into two sets based on a
  predicate function.
}
\section{Corner Cases}{
  if \code{coll} is empty the empty list is returned. If
  all the elements return only true/only false, then one of
  two sublists will be the empty list.
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

