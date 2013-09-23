\name{xVal}
\alias{xVal}
\title{xVal}
\usage{
  xVal(str, val)
}
\arguments{
  \item{str}{a string or symbol.}

  \item{val}{an arbitrary value.}
}
\value{
  Null; this function is used for side-effects.
}
\description{
  Assign a constant value to calling environment.
}
\section{Corner Cases}{
  throws an error if a value already exists called str.
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

