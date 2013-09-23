\name{xForall}
\alias{xForall}
\title{xForall}
\usage{
  xForall(pred, ...)
}
\arguments{
  \item{pred}{an n-ary predicate.}

  \item{...}{n collections.}
}
\value{
  a boolean value.
}
\description{
  Does ever selection of arguments satisfy a predicate?
}
\section{Corner Cases}{
  if any collection is length zero (or no collections are
  given), then False is returned.
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

