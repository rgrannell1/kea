\name{xAsClosure}
\alias{xAsClosure}
\title{xAsClosure}
\usage{
  xAsClosure(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  a function (closure).
}
\description{
  Convert a primitive function to a closure.
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

\section{Corner Cases}{
  xAsClosure does not work for every primitive function
  (for example 'c'), so caution should be taken when using
  this function.
}
\examples{
inst/examples/blank.R
}

