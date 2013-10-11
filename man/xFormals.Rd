\name{xFormals}
\alias{xFormals}
\title{xFormals}
\usage{
  xFormals(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
  A named list, where each element's name is a parameter in
  f and each value is the default value of that parameter.
}
\description{
  Get the parametres and defaults of a function or
  primitive function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its formals. If a parameter has no default, the
  value of the corresponding element in the retun value
  will be the empty symbol, which is identical to
  \code{quote(expr=)}.
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

