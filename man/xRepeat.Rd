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
\code{\link{xCombos}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDo}};
\code{\link{xDrop}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFirst}};
\code{\link{xFlatMap}}; \code{\link{xFlatten}};
\code{\link{xFoldl}}; \code{\link{xFoldListl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFourth}}; \code{\link{xInit}};
\code{\link{xInter}}; \code{\link{xIsEmpty}};
\code{\link{xIsFalse}}; \code{\link{xIsMember}};
\code{\link{xIsNa}}; \code{\link{xIsNan}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xJoin}}; \code{\link{xLast}};
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
\code{\link{xRest}}; \code{\link{xReverse}};
\code{\link{xSecond}}; \code{\link{xSegment}};
\code{\link{xSelect}}; \code{\link{xSetProd}};
\code{\link{xShuffle}}; \code{\link{xSplit}};
\code{\link{xSplitWith}}; \code{\link{xTake}};
\code{\link{xTakeWhile}}; \code{\link{xThird}};
\code{\link{xUnion}}; \code{\link{xUnit}};
\code{\link{xZip}}; \code{\link{xZipWith}}

Other reshaping_functions: \code{\link{xAssoc}};
\code{\link{xDissoc}}; \code{\link{xFlatten}};
\code{\link{xJoin}}; \code{\link{xPermute}};
\code{\link{xReverse}}; \code{\link{xSegment}};
\code{\link{xShuffle}}; \code{\link{xSplit}};
\code{\link{xSplitWith}}; \code{\link{xZip}};
\code{\link{xZipWith}}
}

