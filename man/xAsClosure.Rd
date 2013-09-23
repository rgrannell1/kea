\name{xAsClosure}
\alias{xAsClosure}
\title{xAsClosure}
\usage{
  xAsClosure(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  a function (closure).
}
\description{
  Convert a primitive function to a closure.
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

\section{Corner Cases}{
  xAsClosure does not work for every primitive function
  (for example 'c'), so caution should be taken when using
  this function.
}
\examples{
inst/examples/blank.R
}

