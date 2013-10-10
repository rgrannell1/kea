\name{xFourth}
\alias{xFourth}
\title{xFourth}
\usage{
  xFourth(coll)
}
\arguments{
  \item{coll}{a collection/}
}
\value{
  the fourth value in \code{coll}.
}
\description{
  Return the fourth value in a collection.
}
\section{Corner Cases}{
  throws an error if \code{coll} has less than four
  element; this is because any other corner case would
  violate the functions type-signature.
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

