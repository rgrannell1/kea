\name{xFoldListl}
\alias{xFoldListl}
\title{xFoldListl}
\usage{
xFoldListl(fn, init, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its right argument.}

  \item{init}{an arbitrary value.}

  \item{coll}{a collection.}
}
\value{
a list with its init element being \code{coll}, and
containing \code{length(coll) + 1}.
}
\description{
Successively combine a list of values into a single value
using a binary function (left to right, with an initial
value). Return a list containing each intermediate result,
and the final result.
}
\section{Corner Cases}{
  returns \code{list(init)} if \code{coll} is length-zero.
}
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xCombos}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xDrop}};
\code{\link{xExists}}; \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xFirst}};
\code{\link{xFlatMap}}; \code{\link{xFlatten}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFourth}};
\code{\link{xInit}}; \code{\link{xInter}};
\code{\link{xIsEmpty}}; \code{\link{xIsFalse}};
\code{\link{xIsMember}}; \code{\link{xIsNan}};
\code{\link{xIsNa}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xJoin}};
\code{\link{xLast}}; \code{\link{xLength}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapWhen}}; \code{\link{xMapply}};
\code{\link{xMap}}; \code{\link{xName}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotTrue}};
\code{\link{xPack}}; \code{\link{xPartial}};
\code{\link{xPartition}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xSetProd}};
\code{\link{xShuffle}}; \code{\link{xSplitWith}};
\code{\link{xSplit}}; \code{\link{xTakeWhile}};
\code{\link{xTake}}; \code{\link{xThird}};
\code{\link{xUnion}}; \code{\link{xUnit}};
\code{\link{xZipWith}}; \code{\link{xZip}}

Other folding_functions: \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xReducer}};
\code{\link{xReduce}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFormals}}; \code{\link{xGetKey}};
\code{\link{xIterate}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapWhen}}; \code{\link{xMapply}};
\code{\link{xMap}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xPartial}};
\code{\link{xPartition}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSplitWith}}; \code{\link{xStopwatch}};
\code{\link{xTakeWhile}}; \code{\link{xVectorise}};
\code{\link{xWait}}; \code{\link{xZipWith}}

Other variadic_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xCombos}}; \code{\link{xCompose}};
\code{\link{xDiffer}}; \code{\link{xDissoc}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xDrop}}; \code{\link{xExists}};
\code{\link{xFilter}}, \code{\link{xFilter...}},
\code{\link{xSelect}}, \code{\link{xSelect...}};
\code{\link{xFirst}}; \code{\link{xFlatMap}};
\code{\link{xFlatten}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFourth}}; \code{\link{xImplode}};
\code{\link{xInit}}; \code{\link{xInter}};
\code{\link{xIsEmpty}}; \code{\link{xIsFalse}};
\code{\link{xIsMember}}; \code{\link{xIsNan}};
\code{\link{xIsNa}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xJoin}};
\code{\link{xJuxtapose}}; \code{\link{xLast}};
\code{\link{xLength}}; \code{\link{xLift}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapWhen}}; \code{\link{xMapply}};
\code{\link{xMap}}; \code{\link{xName}};
\code{\link{xNegate}}; \code{\link{xNotFalse}};
\code{\link{xNotNan}}; \code{\link{xNotNa}};
\code{\link{xNotTrue}}; \code{\link{xPack}};
\code{\link{xPartial}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xPred}};
\code{\link{xRecurMap}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xSetProd}};
\code{\link{xShuffle}}; \code{\link{xSignum}};
\code{\link{xSplitWith}}; \code{\link{xSplit}};
\code{\link{xSubString}}; \code{\link{xSucc}};
\code{\link{xTakeWhile}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xThread}};
\code{\link{xUnchars}}; \code{\link{xUnion}};
\code{\link{xUnlines}}; \code{\link{xUnwords}};
\code{\link{xZipWith}}; \code{\link{xZip}}
}

