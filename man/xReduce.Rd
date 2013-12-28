\name{xReduce}
\alias{xReduce}
\title{xReduce}
\usage{
xReduce(fn, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{coll}{a collection.}
}
\value{
an arbitrary value, depending on the function \code{fn}.
}
\description{
Successively combine a list of values into a single value
using a binary function (left to right).
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero, and
  returns the value inside \code{coll} if coll is
  length-one.
}

\section{Short-Circuiting}{
  The fold family of functions include an optional
  mechanism of prematurely returning an answer, similar to
  the \code{return()} function in base R. By calling
  \code{Return} with the value you want \code{fn} to yield
  instantly, the fold/reduce is halted, and that value is
  yielded as the result of that proceedure. This allows
  fold/reduce to have sub-O(n) efficiency.

  \code{Return()} essentially acts as a higher-order return
  function, that breaks "further" than the normal return
  statement.

  See 'Examples' for an example of short-circuiting in use.
}
\examples{

# 1. the archetypal example of Reduce; summing a vector.

xReduce("+", 1:10)

55

# 2. another associative operator; using rbind to join several
#    vectors into one matrix.

xReduce...(
    rbind,
    c(1, 2),
    c(3, 4))
)

matrix(1:4, 2, 2)
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
\code{\link{xRecurMap}}; \code{\link{xReducer}};
\code{\link{xReject}}; \code{\link{xRepeat}};
\code{\link{xRest}}; \code{\link{xReverse}};
\code{\link{xSecond}}; \code{\link{xSegment}};
\code{\link{xSelect}}; \code{\link{xSetProd}};
\code{\link{xShuffle}}; \code{\link{xSplit}};
\code{\link{xSplitWith}}; \code{\link{xTake}};
\code{\link{xTakeWhile}}; \code{\link{xThird}};
\code{\link{xUnion}}; \code{\link{xUnit}};
\code{\link{xZip}}; \code{\link{xZipWith}}

Other folding_functions: \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xReducer}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
\code{\link{xGet}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xLimit}}; \code{\link{xLocate}};
\code{\link{xLocater}}; \code{\link{xMap}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapply}}; \code{\link{xMapWhen}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xPartial}}; \code{\link{xPartition}};
\code{\link{xPartitionWith}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReducer}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xStopWatch}};
\code{\link{xTakeWhile}}; \code{\link{xVectorise}};
\code{\link{xWait}}; \code{\link{xZipWith}}
}

