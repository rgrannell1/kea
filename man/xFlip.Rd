\name{xFlip}
\alias{xFlip}
\title{xFlip}
\usage{
  xFlip(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  a function of the same arity as \code{fn}.
}
\description{
  Reverse the formal parametres of a function.
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

