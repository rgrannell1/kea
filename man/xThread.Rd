\name{xThread}
\alias{xThread}
\title{xThread}
\usage{
  xThread(init, ...)
}
\arguments{
  \item{init}{an arbitrary value}

  \item{...}{several unary functions.}
}
\value{
  a list.
}
\description{
  Iteratively apply a value to list of functions.
}
\section{Corner Cases}{
  q returns the empty list if \code{coll} is length-zero.
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

