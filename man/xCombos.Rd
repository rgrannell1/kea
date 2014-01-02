\name{xCombos}
\alias{xCombos}
\alias{xCombos...}
\title{xCombos}
\usage{
xCombos(num, coll)

xCombos...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
a list of lists, with each list containing \code{num}
elements.
}
\description{
Generate all ways of choosing several elements from a
collection.
}
\section{Corner Cases}{
  returns the empty list if \code{num} is zero.
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
Other combinatoric_functions: \code{\link{xSetProd}},
\code{\link{xSetProd...}}

Other set_functions: \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}
}

