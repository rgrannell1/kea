\name{xSetProd}
\alias{xSetProd}
\alias{xSetProd...}
\title{xSetProd}
\usage{
xSetProd(colls)

xSetProd...(...)
}
\arguments{
  \item{colls}{n collections.}

  \item{...}{see above.}
}
\value{
a list of n-element lists.
}
\description{
Get the cartesian product of several collections.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
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
\examples{

# 1. xSetProd with xMap, xMapply or xReduce are
#    generally very nice replacements for loops.

xMapply(
	(ith : jth) := {
		ith^2 + jth^2
	},
	xSetProd...(1:4, 1:4)
)

# is roughly the same as base R's...

res <- list()

for (ith in 1:4) {
	for (jth in 1:4) {
		res <- c(res, ith^2 + jth^2)
	}
}

res
}
\seealso{
Other combinatoric_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}

Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}
}

