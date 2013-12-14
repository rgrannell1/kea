\name{xRepeat}
\alias{xRepeat}
\title{xRepeat}
\usage{
xRepeat(num, coll)
}
\arguments{
  \item{num}{a nonnegative positive number}

  \item{coll}{a collection}
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
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xContains}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDrop}};
\code{\link{xFirst}}; \code{\link{xFlatten}};
\code{\link{xFourth}}; \code{\link{xInit}};
\code{\link{xInter}}; \code{\link{xIsEmpty}};
\code{\link{xIsFalse}}; \code{\link{xIsNan}};
\code{\link{xIsNa}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xLast}};
\code{\link{xLength}}; \code{\link{xName}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotTrue}};
\code{\link{xPack}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xShuffle}};
\code{\link{xSplit}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}
}

