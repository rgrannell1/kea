\name{xReducer}
\alias{xReducer}
\title{xReducer}
\usage{
xReducer(fn, coll)
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
using a binary function (right to left).
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
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xRepeat}}; \code{\link{xRest}};
\code{\link{xReverse}}; \code{\link{xSecond}};
\code{\link{xSegment}}; \code{\link{xSelect}};
\code{\link{xSetProd}}; \code{\link{xShuffle}};
\code{\link{xSplitWith}}; \code{\link{xSplit}};
\code{\link{xTakeWhile}}; \code{\link{xTake}};
\code{\link{xThird}}; \code{\link{xUnion}};
\code{\link{xUnit}}; \code{\link{xZipWith}};
\code{\link{xZip}}

Other folding_functions: \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xReduce}}

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
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xStopWatch}}; \code{\link{xTakeWhile}};
\code{\link{xVectorise}}; \code{\link{xWait}};
\code{\link{xZipWith}}
}

