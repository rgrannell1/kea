\name{xZipNames}
\alias{xZipNames}
\alias{xZipNames...}
\title{xZipNames}
\usage{
xZipNames(colls)

xZipNames...(...)
}
\arguments{
  \item{colls}{a list or pairlist of two-element lists or
  pairlists, with the first element being a string and the
  second element being an arbitrary value. The string part
  of each sublist is used as the name in the output list,
  and the remaining part is used as the value associated
  with that key.}

  \item{...}{see above.}
}
\value{
A named list.
}
\description{
Convert a list of name, value pairs into a named list.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

# 1. turn a csv of country names and median ages into
#    a named list of countires, with values being the median age.

csv <- "Monaco,48.9,Japan,44.6,Italy,44.3,Germany,43.7,Jersey,43.4"

x_(csv) $ xExplode(',') $ xChunk(2) $ x_ZipNames()

# list(
#    Monaco =
#        48.9,
#    Japan =
#        44.6,
#    Italy =
#        44.3,
#    Germany =
#        43.7,
#    Jersey =
#        43.4
# )

x_(csv) $ xExplode(',') $ xChunk(2) $ x_ZipNames()
}
\seealso{
Other name_functions: \code{\link{xAsNamed}};
\code{\link{xGetKey}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}

Other reshaping_functions: \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xChunk}},
\code{\link{xChunk...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}},
\code{\link{xReverse...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSortBy}},
\code{\link{xSortBy...}}; \code{\link{xSplitAt}},
\code{\link{xSplitAt...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xTabulate}},
\code{\link{xTabulate...}}; \code{\link{xUnzipNames}},
\code{\link{xUnzipNames...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

