\name{xVectorise}
\alias{xVectorise}
\alias{xVectorize}
\title{xVectorise}
\usage{
xVectorise(fn)

xVectorize(fn)
}
\arguments{
  \item{fn}{a unary function.}
}
\value{
A unary function of coll.
}
\description{
Convert a function on one value to a function on
collections of values.
}
\details{
\code{xVectorise} is a convenience function that partially
applies \code{xMap} with a function, creating a vectorised
version of that function.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}}

Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}
}

