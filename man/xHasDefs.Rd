\name{xHasDefs}
\alias{xHasDefs}
\title{xHasDefs}
\usage{
  xHasDefs(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  a named vector of true of false value of the same length
  as the arity of \code{fn}.
}
\description{
  Return a boolean vector showing which parameters of a
  function has defaults.
}
\section{Corner Cases}{
  if \code{fn} is nullary \code{logical(0)} is returned.
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
  accept.  Unary (1), Binary (2), Trinary (3), Quaternary
  (4), ...., Variadic (Inf).

  \bold{Number}: a double, integer or complex number,
  unless stated otherwise.

  \bold{Higher Order Function}: a function that takes a
  function as one of its arguments, and/or returns a
  function.
}
\examples{
inst/examples/blank.R
}

