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

