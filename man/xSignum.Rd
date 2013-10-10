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

