\name{xJuxtapose}
\alias{xJuxtapose}
\title{xJuxtapose}
\usage{
  xJuxtapose(...)
}
\arguments{
  \item{fns}{a list or pairlist of functions.}
}
\value{
  a variadic function.
}
\description{
  Create a function that applies an argument to a list of
  underlying functions.
}
\section{Corner Cases}{

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

