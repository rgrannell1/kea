\name{xChunk}
\alias{xChunk}
\alias{xChunk...}
\title{xChunk}
\usage{
xChunk(num, coll)

xChunk...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number. The desired length
  of each group of elements.}

  \item{coll}{a collection. The collections to divide into
  groups.}

  \item{...}{see above.}
}
\value{
A list of lists.
}
\description{
Divide a collection into segments of fixed length.
}
\section{Corner Cases}{
  The final list in the return value will have less than
  \bold{num} elements if \code{length(coll)} is not evenly
  divisible by \bold{num}.  if \bold{coll} is length-zero,
  the empty list is returned.
}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

