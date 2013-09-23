\name{xCompose}
\alias{xCompose}
\title{xCompose}
\usage{
  xCompose(fn1, fn2)
}
\arguments{
  \item{fn1}{a unary function, or a symbol or name
  identifying such a function.}

  \item{fn2}{a unary function, or a symbol or name
  identifying such a function.}
}
\value{
  A function with the same parameters as \code{g}.
}
\description{
  Compose two functions.
}
\details{
  Unlike some other functions that return functions in
  Arrow, \code{xCompose} preserves the parameters of one of
  its input functions (the right function).  This is not
  always possible for some other functions, but it is
  included in \code{xCompose} as the function is ubiquitous
  and should be as powerful as possible, even at the risk
  of making Arrow less consistent.
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

