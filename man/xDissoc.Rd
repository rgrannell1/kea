\name{xDissoc}
\alias{xDissoc}
\title{xDissoc}
\usage{
  xDissoc(coll)
}
\arguments{
  \item{coll}{a list or pairlist of list or pairlist pairs,
  with the first element being a string and the second
  element being any value.}
}
\value{
  a named list.
}
\description{
  Split a named list into a list of name: value lists.
}
\section{Corner Cases}{
  returns \code{list()} if \code{coll} is length-zero.
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

