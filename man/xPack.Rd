\name{xPack}
\alias{xPack}
\title{xPack}
\usage{
xPack(coll)
}
\arguments{
  \item{coll}{a collection.}
}
\value{
returns a list of elements in \code{coll}, with all
length-zero values removed.
}
\description{
Remove all length-zero values from a collection.
}
\section{Corner Cases}{
  Returns the emty list if \code{coll} is length-zero, or
  all elements in \code{coll} are length-zero.
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
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xShuffle}};
\code{\link{xSplit}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}
}

