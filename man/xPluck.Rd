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

