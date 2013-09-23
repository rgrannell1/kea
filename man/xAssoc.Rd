\name{xAssoc}
\alias{xAssoc}
\title{xAssoc}
\usage{
  xAssoc(coll)
}
\arguments{
  \item{coll}{a list or pairlist of list or pairlist pairs,
  with the first element being a string and the second
  element being any value.}
}
\value{
  a named list.
}
\description{
  Convert a list of name : value lists into a named list.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
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

