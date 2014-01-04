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
returns a list of equal length to the shortest input
collection, with each element being an n-element list.
}
\description{
Generate a list of n-element lists from n collections.
}
\section{Corner Cases}{
  the empty list is returned if the shortest collection has
  length-zero, or no collections are included.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}}
}

