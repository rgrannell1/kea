\name{xIsVal}
\alias{xIsVal}
\title{xIsVal}
\usage{
  xIsVal(str)
}
\arguments{
  \item{str}{a string or symbol.}
}
\value{
  a boolean value.
}
\description{
  Is a name binding locked?
}
\section{Corner Cases}{
  If a variable isn't assigned, then \code{false} is
  returned.
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

