\name{xReducer}
\alias{xReducer}
\title{xReducer}
\usage{
  xReducer(fn, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{coll}{a collection.}
}
\value{
  an arbitrary value, depending on the function \code{fn}.
}
\description{
  Fold a function over a collection from right to left.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero, and
  returns the value inside \code{coll} if coll is
  length-one.
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

