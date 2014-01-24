\name{xFlatten}
\alias{xFlatten}
\alias{xFlatten...}
\title{xFlatten}
\usage{
xFlatten(num, coll)

xFlatten...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole-number.}

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
\code{num} specifies the maximum allowed depth in the
returned list;
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
  Flattening to infinite doesn't affect the depth of the
  output list.  Flattening to one level flattens the list
  fully.
}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

