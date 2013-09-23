\name{xApply}
\alias{xApply}
\title{xApply}
\usage{
  xApply(fn, coll)
}
\arguments{
  \item{fn}{an function of any arity.}

  \item{coll}{a list or pairlist. This may be named, but
  all names must be parameters of \code{fn}.}
}
\value{
  the return value of \code{fn}.
}
\description{
  Call a function with a list of arguments.
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

