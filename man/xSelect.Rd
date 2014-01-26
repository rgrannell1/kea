\name{xSelect}
\alias{xSelect}
\alias{xSelect...}
\title{xSelect}
\usage{
xSelect(pred, coll)

xSelect...(pred, ...)
}
\arguments{
  \item{pred}{a predicate. The function used to test each
  element of the input collection.}

  \item{coll}{a collection. The collection to remove
  elements from.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Include all elements from a collection matching a
predicate.
}
\details{
\bold{xSelect} applies a predicate function such as
\bold{is.integer} or \bold{is.null} to each element of a
collection. The elements for which the predicate returned
true are kept, and the remaining elements are removed from
the collection.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero, or
  no match is found. If the predicate returns a non-logical
  value an error is thrown. If an na value is returned by
  the predicate it is treated as a false value.
}
\examples{

}
\seealso{
Other filtering_functions: \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartition}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xRemoveEmpty}}, \code{\link{xRemoveEmpty...}};
\code{\link{xRemoveNan}}; \code{\link{xRemoveNa}};
\code{\link{xRemoveNull}}, \code{\link{xRemoveNull...}}
}

