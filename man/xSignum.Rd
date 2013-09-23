\name{xSignum}
\alias{xSignum}
\title{xSignum}
\usage{
  xSignum(nums)
}
\arguments{
  \item{nums}{a vector of non-complex numbers.}
}
\value{
  a vector of elements in the set \code{-1, 0, +1}.
}
\description{
  Get the sign of a vector of numbers.
}
\section{Corner Cases}{
  If \code{nums} is lenth-zero then the unit of that vector
  is returned.  The sign of zero is zero.
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

