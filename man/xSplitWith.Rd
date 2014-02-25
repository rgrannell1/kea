\name{xSplitBy}
\alias{xSplitBy}
\alias{xSplitBy...}
\title{xSplitWith}
\usage{
xSplitBy(pred, coll)

xSplitBy...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list of lists.
}
\description{
Divide a collection into groups determined by a predicate.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL
}
\seealso{
Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xRecycle}},
\code{\link{xRecycle...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSortBy}},
\code{\link{xSortBy...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

