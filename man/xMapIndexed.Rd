\name{xMapIndexed}
\alias{xMapIndexed}
\alias{xMapIndexed...}
\title{xMapIndexed}
\usage{
xMapIndexed(fn, coll)

xMapIndexed...(fn, ...)
}
\arguments{
  \item{fn}{a binary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Apply a binary function to each element of a collection and
its index.
}
\section{Corner Cases}{
  Returns the empty list is \bold{coll} is length-zero.
}
\examples{

}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

