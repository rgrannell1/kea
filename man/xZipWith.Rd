\name{xZipWith}
\alias{xZipWith}
\alias{xZipWith...}
\title{xZipWith}
\usage{
xZipWith(fn, colls)

xZipWith...(fn, ...)
}
\arguments{
  \item{fn}{an n-ary function, or a symbol or name
  identifying such a function.}

  \item{colls}{n-vectors, lists or pairlists.}

  \item{...}{see above.}
}
\value{
returns a list of equal length to the shortest input
collection.
}
\description{
Generate a list of n-element lists from n collections, and
apply a function to each n-element list.
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
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

