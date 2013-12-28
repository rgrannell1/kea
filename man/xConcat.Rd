\name{xConcat}
\alias{xConcat}
\title{xConcat}
\usage{
xConcat(colls)
}
\arguments{
  \item{colls}{several collections.}
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
\code{\link{xLast}}; \code{\link{xLength}};
\code{\link{xLocate}}; \code{\link{xLocater}};
\code{\link{xMap}}; \code{\link{xMapIndexed}};
\code{\link{xMapMany}}; \code{\link{xMapply}};
\code{\link{xMapWhen}}; \code{\link{xName}};
\code{\link{xNotFalse}}; \code{\link{xNotNa}};
\code{\link{xNotNan}}; \code{\link{xNotTrue}};
\code{\link{xPack}}; \code{\link{xPartial}};
\code{\link{xPartition}}; \code{\link{xPartitionWith}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReduce}}; \code{\link{xReducer}};
\code{\link{xReject}}; \code{\link{xRepeat}};
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
\code{\link{xPermute}}; \code{\link{xRepeat}};
\code{\link{xReverse}}; \code{\link{xSegment}};
\code{\link{xShuffle}}; \code{\link{xSplit}};
\code{\link{xSplitWith}}; \code{\link{xZip}};
\code{\link{xZipWith}}
}

