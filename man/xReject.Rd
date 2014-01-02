\name{xReject}
\alias{xReject}
\alias{xReject...}
\title{xReject}
\usage{
xReject(pred, coll)

xReject...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Remove all elements from a collection matching a predicate.
}
\section{Uses}{
  The uses for the reject function are very similar to
  those for select.
}

\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
  Throws an error if pred returns a non-boolean value.
}

\section{Appendix-Variadic functions}{
  Many arrow functions come in two forms:

  \itemize{ \item{\bold{xMethod:}}{ These functions do not
  collect arguments with the '...' parametre.}
  \item{\bold{xMethod...:}}{ These functions collect
  arguments with the '...' parametre.} }

  The xMethod... form is somewhat shorter to write, but it
  is mainly used when the arrow function will take a fixed
  number of arguments. An ideal use-case would be writing

  \code{ xMap...( xGetKey('a'), list(a = 1, b = 1), list(a
  = 2, b = 1))}

  instead of

  \code{ xMap( xGetKey('a'), list( list(a = 1, b = 1),
  list(a = 2, b = 1)) )}

  For other applications the xMethod form of the function
  is more likely to be useful:

  \code{nums <- (1:10) / 10}

  \code{xMap(sqrt, nums)}

  As opposed to

  \code{xApply(xMap..., list(sqrt, nums))}

  Arrow methods also have variadic and non-variadic forms
  in many cases.
}
\seealso{
Other filtering_functions: \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartition}}
}

