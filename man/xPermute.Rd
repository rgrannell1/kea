\name{xPermute}
\alias{xPermute}
\alias{xPermute...}
\title{xPermute}
\usage{
xPermute(nums, colls)

xPermute...(nums, ...)
}
\arguments{
  \item{nums}{a collection of whole numbers. The indices by
  which to permute the input collections.}

  \item{colls}{several collections of equal length to
  \bold{nums}.}

  \item{...}{see above.}
}
\value{
A list of collections.
}
\description{
Permute several collections using indices.
}
\details{
\bold{xPermute} allows several collections to be rearranged
in the same way simultaneously.

\code{colls <- list(ids = list(15, 1, 12), ages = list(17,
29, 24), weights = list(80, 82, 76))}

\code{xPermute((order %of% xAsInteger)( xFirstOf(colls) ),
colls)}
}
\section{Corner Cases}{
  returns the empty list is \bold{nums} is length-zero.
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

