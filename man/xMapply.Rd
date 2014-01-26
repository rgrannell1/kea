\name{xMapply}
\alias{xMapply}
\alias{xMapply...}
\title{xMapply}
\usage{
xMapply(fn, coll)

xMapply...(fn, ...)
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
  Returns the empty list is \bold{coll} is length-zero.
}
\examples{

NULL
}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

