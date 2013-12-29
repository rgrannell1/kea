\name{xFlatMap}
\alias{xFlatMap}
\alias{xFlatMap...}
\title{xFlatMap}
\usage{
xFlatMap(fn, coll)

xFlatMap...(fn, ...)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Concatenate the results of applying a function to each
element of a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other collection_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xDo}},
\code{\link{xDo...}}; \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xExists}},
\code{\link{xExists...}}; \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xFoldList}},
\code{\link{xFoldList...}}, \code{\link{xFoldListl}},
\code{\link{xFoldListl...}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}, \code{\link{xFoldr...}};
\code{\link{xForall}}, \code{\link{xForall...}};
\code{\link{xFourth}}, \code{\link{xFourth...}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xInter}}, \code{\link{xInter...}};
\code{\link{xIsEmpty}}, \code{\link{xIsEmpty...}};
\code{\link{xIsFalse}}, \code{\link{xIsFalse...}};
\code{\link{xIsMember}}, \code{\link{xIsMember...}};
\code{\link{xIsNan}}, \code{\link{xIsNan...}};
\code{\link{xIsNa}}, \code{\link{xIsNa...}};
\code{\link{xIsNull}}, \code{\link{xIsNull...}};
\code{\link{xIsTrue}}, \code{\link{xIsTrue...}};
\code{\link{xJoin}}, \code{\link{xJoin...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xLength}}, \code{\link{xLength...}};
\code{\link{xLocater}}, \code{\link{xLocater...}};
\code{\link{xLocate}}, \code{\link{xLocate...}},
\code{\link{xLocatel}}, \code{\link{xLocatel...}};
\code{\link{xMapIndexed}}, \code{\link{xMapIndexed...}};
\code{\link{xMapMany}}, \code{\link{xMapMany...}};
\code{\link{xMapWhen}}, \code{\link{xMapWhen...}};
\code{\link{xMapply}}, \code{\link{xMapply...}};
\code{\link{xMap}}, \code{\link{xMap...}};
\code{\link{xName}}; \code{\link{xNotFalse}},
\code{\link{xNotFalse...}}; \code{\link{xNotNan}},
\code{\link{xNotNan...}}; \code{\link{xNotNa}},
\code{\link{xNotNa...}}; \code{\link{xNotTrue}},
\code{\link{xNotTrue...}}; \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xPartition}};
\code{\link{xPermute}}, \code{\link{xPermute...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xPoll}}, \code{\link{xPoll...}};
\code{\link{xRecurMap}}, \code{\link{xRecurMap...}};
\code{\link{xReducer}}, \code{\link{xReducer...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xRepeat}}, \code{\link{xRepeat...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xReverse}}; \code{\link{xSecond}},
\code{\link{xSecond...}}; \code{\link{xSegment}},
\code{\link{xSegment...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xSplit}},
\code{\link{xSplit...}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xTake}},
\code{\link{xTake...}}; \code{\link{xThird}},
\code{\link{xThird...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUnit}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}

Other higher_order_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xArity}};
\code{\link{xAsClosure}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDo}}, \code{\link{xDo...}};
\code{\link{xDropWhile}}, \code{\link{xDropWhile...}};
\code{\link{xExists}}, \code{\link{xExists...}};
\code{\link{xFilter}}, \code{\link{xFilter...}},
\code{\link{xSelect}}, \code{\link{xSelect...}};
\code{\link{xFlip}}; \code{\link{xFoldList}},
\code{\link{xFoldList...}}, \code{\link{xFoldListl}},
\code{\link{xFoldListl...}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}, \code{\link{xFoldr...}};
\code{\link{xForall}}, \code{\link{xForall...}};
\code{\link{xFormals}}; \code{\link{xGet}};
\code{\link{xIterate}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xLimit}};
\code{\link{xLocater}}, \code{\link{xLocater...}};
\code{\link{xLocate}}, \code{\link{xLocate...}},
\code{\link{xLocatel}}, \code{\link{xLocatel...}};
\code{\link{xMapIndexed}}, \code{\link{xMapIndexed...}};
\code{\link{xMapMany}}, \code{\link{xMapMany...}};
\code{\link{xMapWhen}}, \code{\link{xMapWhen...}};
\code{\link{xMapply}}, \code{\link{xMapply...}};
\code{\link{xMap}}, \code{\link{xMap...}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xPartition}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xPoll}},
\code{\link{xPoll...}}; \code{\link{xRecurMap}},
\code{\link{xRecurMap...}}; \code{\link{xReducer}},
\code{\link{xReducer...}}; \code{\link{xReduce}},
\code{\link{xReduce...}}, \code{\link{xReducel}},
\code{\link{xReducel...}}; \code{\link{xReject}},
\code{\link{xReject...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xStopwatch}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}};
\code{\link{xWait}}; \code{\link{xZipWith}},
\code{\link{xZipWith...}}

Other mapping_functions: \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xRecurMap}},
\code{\link{xRecurMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}

Other variadic_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xDo}},
\code{\link{xDo...}}; \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xExists}},
\code{\link{xExists...}}; \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xFoldList}},
\code{\link{xFoldList...}}, \code{\link{xFoldListl}},
\code{\link{xFoldListl...}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}, \code{\link{xFoldr...}};
\code{\link{xForall}}, \code{\link{xForall...}};
\code{\link{xFourth}}, \code{\link{xFourth...}};
\code{\link{xImplode}}, \code{\link{xImplode...}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xInter}}, \code{\link{xInter...}};
\code{\link{xIsEmpty}}, \code{\link{xIsEmpty...}};
\code{\link{xIsFalse}}, \code{\link{xIsFalse...}};
\code{\link{xIsMember}}, \code{\link{xIsMember...}};
\code{\link{xIsNan}}, \code{\link{xIsNan...}};
\code{\link{xIsNa}}, \code{\link{xIsNa...}};
\code{\link{xIsNull}}, \code{\link{xIsNull...}};
\code{\link{xIsTrue}}, \code{\link{xIsTrue...}};
\code{\link{xJoin}}, \code{\link{xJoin...}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xLength}}, \code{\link{xLength...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xLocater}}, \code{\link{xLocater...}};
\code{\link{xLocate}}, \code{\link{xLocate...}},
\code{\link{xLocatel}}, \code{\link{xLocatel...}};
\code{\link{xMapIndexed}}, \code{\link{xMapIndexed...}};
\code{\link{xMapMany}}, \code{\link{xMapMany...}};
\code{\link{xMapWhen}}, \code{\link{xMapWhen...}};
\code{\link{xMapply}}, \code{\link{xMapply...}};
\code{\link{xMap}}, \code{\link{xMap...}};
\code{\link{xName}}; \code{\link{xNegate}},
\code{\link{xNegate...}}; \code{\link{xNotFalse}},
\code{\link{xNotFalse...}}; \code{\link{xNotNan}},
\code{\link{xNotNan...}}; \code{\link{xNotNa}},
\code{\link{xNotNa...}}; \code{\link{xNotTrue}},
\code{\link{xNotTrue...}}; \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xPartition}};
\code{\link{xPermute}}, \code{\link{xPermute...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xPoll}}, \code{\link{xPoll...}};
\code{\link{xPred}}, \code{\link{xPred...}};
\code{\link{xRecurMap}}, \code{\link{xRecurMap...}};
\code{\link{xReducer}}, \code{\link{xReducer...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xRepeat}}, \code{\link{xRepeat...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xReverse}}; \code{\link{xSecond}},
\code{\link{xSecond...}}; \code{\link{xSegment}},
\code{\link{xSegment...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xShuffle}},
\code{\link{xShuffle...}}; \code{\link{xSignum}},
\code{\link{xSignum...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xSplit}},
\code{\link{xSplit...}}; \code{\link{xSubString}},
\code{\link{xSubString...}}; \code{\link{xSucc}},
\code{\link{xSucc...}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xTake}},
\code{\link{xTake...}}; \code{\link{xThird}},
\code{\link{xThird...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xUnchars}},
\code{\link{xUnchars...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUnlines}},
\code{\link{xUnlines...}}; \code{\link{xUnwords}},
\code{\link{xUnwords...}}; \code{\link{xZipWith}},
\code{\link{xZipWith...}}; \code{\link{xZip}},
\code{\link{xZip...}}
}

