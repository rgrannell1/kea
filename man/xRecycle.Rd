\name{xRecycle}
\alias{xRecycle}
\alias{xRecycle...}
\title{xRecycle}
\usage{
xRecycle(colls)

xRecycle...(...)
}
\arguments{
  \item{colls}{a collection of collections. The collection
  to convert to a non-ragged form.}

  \item{...}{see above.}
}
\value{
A list of lists.
}
\description{
Make every subcollection in a collection the same length by
cyclically reusing elements.
}
\details{
xRecycle takes a ragged collection - a collection of
collections of varying length - and recycles elements to
make the shorter collections the same length as the longest
collection.

\code{xRecycle...( list('a', 'b'), 1:3 )}

\code{list(list("a", "b", "a"), list(1L, 2L, 3L))}
}
\section{Corner Cases}{
  Returns the empty list if \bold{colls} is length-zero, or
  if a collection in \bold{colls} is length-zero.
}
\seealso{
Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xRepeat}},
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

