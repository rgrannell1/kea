\name{xDissoc}
\alias{xDissoc}
\alias{xDissoc...}
\title{xDissoc}
\usage{
xDissoc(colls)

xDissoc...(...)
}
\arguments{
  \item{colls}{a list or pairlist of list or pairlist
  pairs, with the first element being a string and the
  second element being any value.}

  \item{...}{see above.}
}
\value{
A named list.
}
\description{
Split a named list into a list of name: value list pairs.
}
\section{Corner Cases}{
  Returns \code{list()} if \code{colls} is length-zero.
}
\examples{

NULL

}
\seealso{
Other name_functions: \code{\link{xAsNamed}};
\code{\link{xAssoc}}, \code{\link{xAssoc...}};
\code{\link{xGetKey}}; \code{\link{xPluck}},
\code{\link{xPluck...}}

Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

