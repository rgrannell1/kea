\name{xAsFunction}
\alias{xAsFunction}
\alias{xAsFunction...}
\title{xAsFunction}
\usage{
xAsFunction(coll)

xAsFunction...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
a function that takes one or more indices.
}
\description{
Convert a collection to a function that takes an index.
}
\section{Corner Cases}{
  returns the empty list if \code{coll is length-zero}.
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
\examples{

}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}; \code{\link{xWait}}
}

