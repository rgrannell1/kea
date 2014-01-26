\name{xMapMany}
\alias{xMapMany}
\alias{xMapMany...}
\title{xMapMany}
\usage{
xMapMany(fn, colls)

xMapMany...(fn, ...)
}
\arguments{
  \item{fn}{a n-ary function.}

  \item{colls}{n collections.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Map a function across many collections simultaneously.
}
\section{Corner Cases}{
  Returns the empty list if any collection is length-zero.
  If the collection lengths are not equal then elements are
  recycled in the shorter collections.
}
\examples{

# 1. Multiply the corresponding elements of two lists.

xMapMany...('*', sample(0:9), sample(0:9))

}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

