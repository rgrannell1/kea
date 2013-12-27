\name{xCombos}
\alias{xCombos}
\title{xCombos}
\usage{
xCombos(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
a list of lists, with each list containing \code{num}
elements.
}
\description{
Generate all ways of choosing several elements from a
collection.
}
\section{Corner Cases}{
  returns the empty list if \code{num} is zero.
}
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xDiffer}}; \code{\link{xDissoc}};
\code{\link{xDo}}; \code{\link{xDrop}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFirst}}; \code{\link{xFlatMap}};
\code{\link{xFlatten}}; \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFourth}};
\code{\link{xInit}}; \code{\link{xInter}};
\code{\link{xIsEmpty}}; \code{\link{xIsFalse}};
\code{\link{xIsMember}}; \code{\link{xIsNa}};
\code{\link{xIsNan}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xLast}};
\code{\link{xLength}}; \code{\link{xLocate}};
\code{\link{xLocater}}; \code{\link{xMap}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapply}}; \code{\link{xMapWhen}};
\code{\link{xName}}; \code{\link{xNotFalse}};
\code{\link{xNotNa}}; \code{\link{xNotNan}};
\code{\link{xNotTrue}}; \code{\link{xPack}};
\code{\link{xPartial}}; \code{\link{xPartition}};
\code{\link{xPartitionWith}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReduce}};
\code{\link{xReducer}}; \code{\link{xReject}};
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xSelect}};
\code{\link{xSetProd}}; \code{\link{xShuffle}};
\code{\link{xSplit}}; \code{\link{xSplitWith}};
\code{\link{xTake}}; \code{\link{xTakeWhile}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}; \code{\link{xZip}};
\code{\link{xZipWith}}

Other combinatoric_functions: \code{\link{xSetProd}}
}

