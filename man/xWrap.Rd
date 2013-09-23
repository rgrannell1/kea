\name{xWrap}
\alias{xWrap}
\title{xWrap}
\usage{
  xWrap(fn1, fn2)
}
\arguments{
  \item{fn1}{an arbitrary function.}

  \item{fn2}{a binary function, that takes \code{fn1} as
  its first argument and additional arguments as ith second
  argument.}
}
\value{
  a variadic function.
}
\description{
  Wrap a function within another function
}
\section{Corner Cases}{

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

