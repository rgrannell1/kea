\name{xFlatMap}
\alias{xFlatMap}
\title{xFlatMap}
\usage{
xFlatMap(fn, coll)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Concatenate the results of applying a function to each
element of a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions mapping_functions
collection_functions: \code{\link{xMapIndexed}};
\code{\link{xMapMany}}; \code{\link{xMapWhen}};
\code{\link{xMapply}}; \code{\link{xMap}};
\code{\link{xRecurMap}}
}

