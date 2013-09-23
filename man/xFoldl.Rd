\name{xFoldl}
\alias{xFoldl}
\title{xFoldl}
\usage{
  xFoldl(fn, init, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{init}{an arbitrary value.}

  \item{coll}{a collection.}
}
\value{
  an arbitrary value, depending on the function \code{fn}.
}
\description{
  Fold a function over a collection from left to right with
  an initial left value.
}
\section{Corner Cases}{
  returns \code{init} if \code{coll} is length-zero.
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

