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
A list.
}
\description{
Apply a function to each element of a collection.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

