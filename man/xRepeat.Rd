\name{xRepeat}
\alias{xRepeat}
\alias{xRepeat...}
\title{xRepeat}
\usage{
xRepeat(num, coll)

xRepeat...(num, ...)
}
\arguments{
  \item{num}{a nonnegative positive number}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Repeat a collection several times.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero or
  num is zero.
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
\code{\link{xPermute...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

