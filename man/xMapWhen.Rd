\name{xMapWhen}
\alias{xMapWhen}
\title{xMapWhen}
\usage{
xMapWhen(pred, fn, coll)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{fn}{a unary function.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Selectively apply a function to elements in a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions map_like_functions
collection_functions: \code{\link{xFlatMap}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapply}}; \code{\link{xMap}};
\code{\link{xRecurMap}}
}

