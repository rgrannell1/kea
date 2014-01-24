\name{xSplitWith}
\alias{xSplitWith}
\alias{xSplitWith...}
\title{xSplitWith}
\usage{
xSplitWith(pred, coll)

xSplitWith...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list of two lists.
}
\description{
Split a list into two lists; all the elements before the
first time a predicate returns false, and all the elements
including and after that point.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
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
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

