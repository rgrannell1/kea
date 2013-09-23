\name{xHasDefs}
\alias{xHasDefs}
\title{xHasDefs}
\usage{
  xHasDefs(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  a named vector of true of false value of the same length
  as the arity of \code{fn}.
}
\description{
  Return a boolean vector showing which parameters of a
  function has defaults.
}
\section{Corner Cases}{
  if \code{fn} is nullary \code{logical(0)} is returned.
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

