\name{xSplitWith}
\alias{xSplitWith}
\title{xSplitWith}
\usage{
xSplitWith(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}
}
\value{
a list of two lists.
}
\description{
Split a list into two lists; all the elements before the
first time a predicate returns false, and all the elements
including and after that point.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xCombos}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xDrop}};
\code{\link{xExists}}; \code{\link{xFirst}};
\code{\link{xFlatMap}}; \code{\link{xFlatten}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFourth}}; \code{\link{xInit}};
\code{\link{xInter}}; \code{\link{xIsEmpty}};
\code{\link{xIsFalse}}; \code{\link{xIsMember}};
\code{\link{xIsNan}}; \code{\link{xIsNa}};
\code{\link{xIsNull}}; \code{\link{xIsTrue}};
\code{\link{xJoin}}; \code{\link{xLast}};
\code{\link{xLength}}; \code{\link{xLocater}};
\code{\link{xLocate}}; \code{\link{xMapIndexed}};
\code{\link{xMapMany}}; \code{\link{xMapWhen}};
\code{\link{xMapply}}; \code{\link{xMap}};
\code{\link{xName}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotTrue}}; \code{\link{xPack}};
\code{\link{xPartial}}; \code{\link{xPartitionWith}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xRepeat}};
\code{\link{xRest}}; \code{\link{xReverse}};
\code{\link{xSecond}}; \code{\link{xSegment}};
\code{\link{xSelect}}; \code{\link{xSetProd}};
\code{\link{xShuffle}}; \code{\link{xSplit}};
\code{\link{xTakeWhile}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}; \code{\link{xZipWith}};
\code{\link{xZip}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
\code{\link{xGet}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xLimit}}; \code{\link{xLocater}};
\code{\link{xLocate}}; \code{\link{xMapIndexed}};
\code{\link{xMapMany}}; \code{\link{xMapWhen}};
\code{\link{xMapply}}; \code{\link{xMap}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xPartial}}; \code{\link{xPartitionWith}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xStopWatch}}; \code{\link{xTakeWhile}};
\code{\link{xVectorise}}; \code{\link{xWait}};
\code{\link{xZipWith}}

Other reshaping_functions: \code{\link{xAssoc}};
\code{\link{xDissoc}}; \code{\link{xFlatten}};
\code{\link{xJoin}}; \code{\link{xPermute}};
\code{\link{xRepeat}}; \code{\link{xReverse}};
\code{\link{xSegment}}; \code{\link{xShuffle}};
\code{\link{xSplit}}; \code{\link{xZipWith}};
\code{\link{xZip}}
}

