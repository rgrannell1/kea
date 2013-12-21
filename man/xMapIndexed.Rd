\name{xMapIndexed}
\alias{xMapIndexed}
\title{xMapIndexed}
\usage{
xMapIndexed(fn, coll)
}
\arguments{
  \item{fn}{a binary function.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Apply a binary function to each element of a collection and
its indices.
}
\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions mapping_functions
collection_functions: \code{\link{xFlatMap}};
\code{\link{xMapMany}}; \code{\link{xMapWhen}};
\code{\link{xMapply}}; \code{\link{xMap}};
\code{\link{xRecurMap}}
}

