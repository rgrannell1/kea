\name{xJoin}
\alias{xJoin}
\alias{xJoin...}
\title{xJoin}
\usage{
xJoin(colls)

xJoin...(...)
}
\arguments{
  \item{colls}{several collections.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Concatenate several collections into one collection.
}
\section{Corner Cases}{
  empty collections act as a unit for concatenation;
  concatenating the empty list to another list returns the
  second list, without modification.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

