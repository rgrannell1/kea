\name{xConcat}
\alias{xConcat}
\title{xConcat}
\usage{
  xConcat(...)
}
\arguments{
  \item{coll}{a collection}
}
\value{
  a list.
}
\description{
  Concatenate several collections into one collectino.
}
\section{Corner Cases}{
  empty collections act as a unit for concatenation;
  concatenating the empty list to another list returns the
  second list, without modification.
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

