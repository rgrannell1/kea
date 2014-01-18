\name{xShuffle}
\alias{xShuffle}
\alias{xShuffle...}
\title{xShuffle}
\usage{
xShuffle(coll)

xShuffle...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Randomly rearrange a collection.
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
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xGroup}},
\code{\link{xGroup...}}; \code{\link{xJoin}},
\code{\link{xJoin...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSplitAt}}, \code{\link{xSplitAt...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

