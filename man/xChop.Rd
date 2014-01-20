\name{xChop}
\alias{xChop}
\alias{xChop...}
\title{xChop}
\usage{
xChop(num, coll)

xChop...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list of lists.
}
\description{
Divide a collection into a fixed number of segments.
}
\section{Corner Cases}{
  The final list in the return value will have less than
  \code{num} elements if \code{length(coll)} is not evenly
  divisible into \code{num} collections.  If \code{coll} is
  length-zero, the empty list is returned.
}

\section{Corner Cases}{

}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xDissoc}},
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

