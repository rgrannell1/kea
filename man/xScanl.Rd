\name{xScanl}
\alias{xScanl}
\title{xScanl}
\usage{
  xScanl(fn, init, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its right argument.}

  \item{init}{an arbitrary value.}

  \item{coll}{a collection.}
}
\value{
  a list with its init element being \code{coll}, and
  containing \code{length(coll) + 1}.
}
\description{
  Fold a function over a collection from left to right with
  an initital left value, keeping intermediate values.
}
\section{Corner Cases}{
  returns \code{list(init)} if \code{coll} is length-zero.
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

