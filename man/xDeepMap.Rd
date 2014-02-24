\name{xDeepMap}
\alias{xDeepMap}
\alias{xDeepMap...}
\title{xDeepMap}
\usage{
xDeepMap(fn, coll)

xDeepMap...(fn, ...)
}
\arguments{
  \item{fn}{a unary function. A function to recursively
  apply into a collection.}

  \item{coll}{a collection. The collection to be mapped
  into.}

  \item{...}{see above.}
}
\value{
A list or pairlist.
}
\description{
Recursively map a function into a nested collection,
preserving its structure.
}
\examples{

NULL

}
\seealso{
Other mapping_functions: \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

