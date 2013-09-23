\name{xIsNan}
\alias{xIsNan}
\title{xIsNan}
\usage{
  xIsNan(coll)
}
\arguments{
  \item{coll}{a collection}
}
\value{
  a list.
}
\description{
  Is an element in a collection NaN?
}
\section{Corner Cases}{
  returns True if coll is length-zero.
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

