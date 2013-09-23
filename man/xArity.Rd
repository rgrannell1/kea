\name{xArity}
\alias{xArity}
\title{xArity}
\usage{
  xArity(fn)
}
\arguments{
  \item{fn}{an function of any arity.}
}
\value{
  a positive whole number.
}
\description{
  Return the arity of a function.
}
\section{Corner Cases}{
  If \code{fn} is a variadic function of any kind positive
  infinity is returned.
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

