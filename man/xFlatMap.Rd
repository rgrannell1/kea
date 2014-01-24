\name{xFlatMap}
\alias{xFlatMap}
\alias{xFlatMap...}
\title{xFlatMap}
\usage{
xFlatMap(fn, coll)

xFlatMap...(fn, ...)
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
Concatenate the results of applying a function to each
element of a collection.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

