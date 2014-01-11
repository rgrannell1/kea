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
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xChop}},
\code{\link{xChop...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xGroup}},
\code{\link{xGroup...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

