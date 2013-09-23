\name{xCombinations}
\alias{xCombinations}
\title{xCombinations}
\usage{
  xCombinations(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
  a list of lists, with each list containing \code{num}
  elements.
}
\description{
  Generate all ways of choosing several elements from a
  column.
}
\section{Corner Cases}{
  returns the empty list if \code{num} is zero.
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

