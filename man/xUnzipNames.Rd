\name{xUnzipNames}
\alias{xUnzipNames}
\alias{xUnzipNames...}
\title{xUnzipNames}
\usage{
xUnzipNames(colls)

xUnzipNames...(...)
}
\arguments{
  \item{colls}{a collection of collections, with the first
  element of the inner collection being a string and the
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
\code{\link{xGetKey}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}

Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xOneOf}},
\code{\link{xOneOf...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSortBy}},
\code{\link{xSortBy...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xZipNames}},
\code{\link{xZipNames...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

