\name{xPartial}
\alias{xPartial}
\title{xPartial}
\usage{
  xPartial(fn, coll)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{coll}{a collection}
}
\value{
  A function of equal or lesser arity to \code{fn}.
}
\description{
  Partially apply a function.
}
\section{Corner Cases}{
  Partial application also works for ellipses (eg list(...
  = 1)).
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

