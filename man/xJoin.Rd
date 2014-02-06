\name{xJoin}
\alias{xJoin}
\alias{xJoin...}
\title{xJoin}
\usage{
xJoin(colls)

xJoin...(...)
}
\arguments{
  \item{colls}{a collection of collections. The collections
  to concatenate to each other.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Concatenate several collections into one collection.
}
\section{Corner Cases}{
  Empty collections act as a unit for concatenation;
  concatenating the empty list to another list returns the
  second list, without modification.
}
\examples{

NULL

}
\seealso{
Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSortBy}},
\code{\link{xSortBy...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

