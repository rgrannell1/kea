\name{xZip}
\alias{xZip}
\alias{xZip...}
\title{xZip}
\usage{
xZip(colls)

xZip...(...)
}
\arguments{
  \item{colls}{n-vectors, lists or pairlists.}

  \item{...}{see above.}
}
\value{
Returns a list.
}
\description{
Generate a list of n-element lists from n collections.
}
\section{Corner Cases}{
  The empty list is returned if the shortest collection has
  length-zero, or no collections are included.
}

\section{Variadic Functions

\link{ help_variadic }}{

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
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}}
}

