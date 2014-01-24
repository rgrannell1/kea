\name{xPermute}
\alias{xPermute}
\alias{xPermute...}
\title{xPermute}
\usage{
xPermute(coll, colls)

xPermute...(coll, ...)
}
\arguments{
  \item{coll}{a collection of whole numbers.}

  \item{colls}{several collections of equal length to
  \bold{coll}.}

  \item{...}{see above.}
}
\value{
A list of collections.
}
\description{
Permute several collections using indices.
}
\section{Corner Cases}{
  returns the empty list is \bold{coll} is length-zero.
}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

