\name{xFlatten}
\alias{xFlatten}
\title{xFlatten}
\usage{
xFlatten(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole-number.}

  \item{coll}{a list or pairlist.}
}
\value{
a list.
}
\description{
Flatten a nested list or pairlist to a given depth.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xContains}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDrop}};
\code{\link{xFirst}}; \code{\link{xFourth}};
\code{\link{xInit}}; \code{\link{xInter}};
\code{\link{xIsEmpty}}; \code{\link{xIsFalse}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xLast}}; \code{\link{xLength}};
\code{\link{xName}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotTrue}}; \code{\link{xPack}};
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xShuffle}};
\code{\link{xSplit}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}
}

