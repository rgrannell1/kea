\name{xSecond}
\alias{xSecond}
\title{xSecond}
\usage{
  xSecond(coll)
}
\arguments{
  \item{coll}{a collection}
}
\value{
  the second element in \code{coll}.
}
\description{
  Return the second element in a collection.
}
\section{Corner Cases}{
  throws an error if \code{coll} has less than two
  elements; this is because any other corner case would
  violate the functions type-signature.
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

