\name{xMap}
\alias{xMap}
\alias{xMap...}
\title{xMap}
\usage{
xMap(fn, coll)

xMap...(fn, ...)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Apply a function to each element of a collection.
}
\section{Uses}{
  xMap is used to apply the same modification to every
  element in a collection. For example, xMap can be used to
  convert every integer in a list to a string, to lookup
  every URL in a list of URLs, or to multiply every integer
  in a list by two.
}

\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
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

  \code{ xMap...(\cr xGetKey('a'),\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1))}

  instead of

  \code{ xMap(\cr xGetKey('a'),\cr list(\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1)) )}

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
Other mapping_functions: \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xRecurMap}},
\code{\link{xRecurMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

