\name{xParams}
\alias{xParams}
\title{xParams}
\usage{
  xParams(fn)
}
\arguments{
  \item{fn}{an arbitrary function or primitive function.}
}
\value{
  a character vector/.
}
\description{
  Get the parametre names of a function or primitive
  function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its parameter names.
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

