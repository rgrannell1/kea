\name{xMap}
\alias{xMap}
\title{xMap}
\usage{
xMap(fn, coll)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Apply a function to each element of a collection.
}
\section{Uses}{
  Map is used to apply the same modification to every
  element in a collection. For example, map can be used to
  convert every integer in a list to a string, to lookup
  every URL in a list of URLs, or to multiply every integer
  in a list by two.
}

\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions mapping_functions
collection_functions: \code{\link{xFlatMap}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapWhen}}; \code{\link{xMapply}};
\code{\link{xRecurMap}}
}

