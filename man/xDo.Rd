\name{xDo}
\alias{xDo}
\title{xDo}
\usage{
xDo(fn, coll)
}
\arguments{
  \item{fn}{a unary function, usually side-effectful.}

  \item{coll}{a collection}
}
\value{
a list.
}
\description{
Map (a possibly side-effectful) function over a collection
and discard the results.
}
\section{Uses}{
  \code{xDo} behaves similarily to \code{xMap}; it applies
  a function to every element of a collection. However,
  \code{xDo} is primarily meant for use with side-effectful
  functions, and is more memory efficient for \code{xDo}
  for this task. \code{xDo} can be used for plotting every
  point in a list of xy coordinates, or for printing every
  value in a list. It is also possible to use \code{xDo}
  for side-effectfully updating values, but this can lead
  to stateful, tangled code.
}
\examples{

# It is difficult to give examples for xDo, since it
# is used for side effects.

#1. print the files in your current directory.

xDo(print, list.files('.'))

#    or

x_(list.files('.'))$xDo(print)

#    or even

xDo(print \%of\% list.files, '.')

}
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xCombos}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDrop}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFirst}}; \code{\link{xFlatMap}};
\code{\link{xFlatten}}; \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFourth}};
\code{\link{xInit}}; \code{\link{xInter}};
\code{\link{xIsEmpty}}; \code{\link{xIsFalse}};
\code{\link{xIsMember}}; \code{\link{xIsNa}};
\code{\link{xIsNan}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xJoin}};
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

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFlatMap}}; \code{\link{xFlip}};
\code{\link{xFoldl}}; \code{\link{xFoldListl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFormals}}; \code{\link{xGet}};
\code{\link{xIterate}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xLocate}}; \code{\link{xLocater}};
\code{\link{xMap}}; \code{\link{xMapIndexed}};
\code{\link{xMapMany}}; \code{\link{xMapply}};
\code{\link{xMapWhen}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xPartial}};
\code{\link{xPartition}}; \code{\link{xPartitionWith}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReduce}}; \code{\link{xReducer}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xStopWatch}};
\code{\link{xTakeWhile}}; \code{\link{xVectorise}};
\code{\link{xWait}}; \code{\link{xZipWith}}
}

