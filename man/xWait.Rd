\name{xWait}
\alias{xWait}
\title{xWait}
\usage{
  xWait(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  a function with the same parameters as \code{fn}.
}
\description{
  Delay the evalution of a function after invokation for a
  set amount of time/
}
\section{Corner Cases}{
  if \code{num} is zero then \code{fn} is returned
  untouched.
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

