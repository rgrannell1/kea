\name{xPartial}
\alias{xPartial}
\title{xPartial}
\usage{
xPartial(fn, coll)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{coll}{a collection}
}
\value{
A function of equal or lesser arity to \code{fn}.
}
\description{
Partially apply a function.
}
\section{Corner Cases}{
  Partial application also works for ellipses (eg list(...
  = 1)).
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
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xRepeat}};
\code{\link{xRest}}; \code{\link{xReverse}};
\code{\link{xSecond}}; \code{\link{xSegment}};
\code{\link{xSelect}}; \code{\link{xSetProd}};
\code{\link{xShuffle}}; \code{\link{xSplitWith}};
\code{\link{xSplit}}; \code{\link{xTakeWhile}};
\code{\link{xTake}}; \code{\link{xThird}};
\code{\link{xUnion}}; \code{\link{xUnit}};
\code{\link{xZipWith}}; \code{\link{xZip}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xNot}};
\code{\link{xThread}}; \code{\link{xVectorise}};
\code{\link{xWait}}

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
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xStopWatch}};
\code{\link{xTakeWhile}}; \code{\link{xVectorise}};
\code{\link{xWait}}; \code{\link{xZipWith}}

Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFlip}};
\code{\link{xFormals}}; \code{\link{xIsVariadic}};
\code{\link{xParams}}
}

