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
  \bold{Function}: a closure or primitive function.  A
  symbol or string refering to such a function may also be
  used.

  \bold{Predicate}: A function that returns a logical value
  (unary, unless stated otherwise).  A symbol or string
  refering to such a function may also be used.

  \bold{Collection}: A list, pairlist or a typed vector.

  \bold{Arity}: The number of arguments a function can
  accept.

  \bold{Nullary}: a function with arity zero. \bold{Unary}:
  a function with arity one. \bold{Binary}: a function with
  arity two. \bold{Trinary}: a function with arity three.

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.
}
\examples{
inst/examples/blank.R
}

