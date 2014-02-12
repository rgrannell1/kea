\name{xFlatten}
\alias{xFlatten}
\alias{xFlatten...}
\title{xFlatten}
\usage{
xFlatten(num, coll)

xFlatten...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole-number. The maximum depth
  of collection in the return value.}

  \item{coll}{a list or pairlist.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Unnest a nested list to a given depth.
}
\details{
\bold{xFlatten} flattens nested collections to a specific
depth. \bold{num} specifies the maximum allowed depth in
the returned list. When flattening a (infinitely nested)
collection, \bold{num} equals 1 returns a collection,
\bold{num} equals 2 returns a collection of collections,
and so on.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
  Flattening to infinite doesn't affect the depth of the
  output list.  Flattening to one level flattens the list
  fully.
}
\examples{

NULL

}
\seealso{
Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSortBy}},
\code{\link{xSortBy...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xSplitBy}},
\code{\link{xSplitBy...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

