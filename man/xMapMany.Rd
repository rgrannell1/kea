\name{xMapMany}
\alias{xMapMany}
\title{xMapMany}
\usage{
  xMapMany(fn, ...)
}
\arguments{
  \item{fn}{a n-ary function.}

  \item{...}{n collections.}
}
\value{
  a list.
}
\description{
  Map an n-ary function over n-collections/
}
\section{Corner Cases}{
  returns the empty list if any collection is length-zero.
  If the collection lengths are not equal then elements are
  recycles in the shorter collections.
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

