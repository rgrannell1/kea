\name{xSplitAt}
\alias{xSplitAt}
\alias{xSplitAt...}
\title{xSplitAt}
\usage{
xSplitAt(nums, coll)

xSplitAt...(nums, ...)
}
\arguments{
  \item{nums}{a vector of nonnegative whole numbers.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list of two lists; the first list containing the first
\code{nums} elements of \code{coll}, and the second list
containing the remaining elements \code{coll}.
}
\description{
Split a collection into elements before and upto an index,
and after that index.
}
\section{Corner Cases}{
  If \code{nums} is zero then the first list in the
  returned value is empty.  Likewise, if \code{nums} is
  equal or larger than the length of \code{coll} then the
  second return list is empty. If \code{coll} is length
  zero both lists are empty.
}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

