\name{xReject}
\alias{xReject}
\alias{xReject...}
\title{xReject}
\usage{
xReject(pred, coll)

xReject...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Remove all elements from a collection matching a predicate.
}
\details{
\bold{xReject} applies a predicate function such as
\bold{is.integer} or \bold{is.null} to each element of a
collection. The elements for which the predicate returned
false are kept, and the remaining elements are removed from
the collection.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero, or
  no match is found. If the predicate returns a non-logical
  value an error is thrown. If an na value is returned by
  the predicate it is treated as a false value.
}
\seealso{
Other filtering_functions: \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartition}};
\code{\link{xRemoveEmpty}}, \code{\link{xRemoveEmpty...}};
\code{\link{xRemoveNan}}; \code{\link{xRemoveNa}};
\code{\link{xRemoveNull}}, \code{\link{xRemoveNull...}};
\code{\link{xSelect}}, \code{\link{xSelect...}}
}

