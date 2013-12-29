\name{xApply}
\alias{xApply}
\title{xApply}
\usage{
xApply(fn, coll)
}
\arguments{
  \item{fn}{an function of any arity.}

  \item{coll}{a list or pairlist. This may be named, but
  all names must be parametres of \code{fn}.}
}
\value{
the return value of \code{fn}.
}
\description{
Invoke a function with a collection of arguments.
}
\section{Uses}{
  \code{xApply} is an adaptor function that allows variadic
  functions to be called with a variable number of
  arguments at runtime.  For example, if \code{xJoin} is
  invoked directly it has to be given a fixed number of
  lists to concatenate.  If \code{xJoin} is invoked with
  \code{xApply} then xJoin can be invoked with any number
  of lists dynamically.
}
\examples{

# 1. call a variadic function with xApply

nums <- c(1, 5,2,3)

xApply(sum, nums)

11

# normally, variadic functions have to be called with
# a fixed number of arguments, as in

a = 1; b = 5; c = 2; d = 3

sum(a, b, c, d)

11

# which cannot be changed at runtime.

# 2. Reduce and xApply can often be used to solve the
#    same problems.

xReduce('+', 1:10)

55

xApply(sum, 1:10)

55

# 3. Rbind and xApply can be used to build up a matrix
#    from vectors.

xApply...(rbind, list(1, 2), list(3, 4), list(5, 6))

structure(
    list(1, 3, 5, 2, 4, 6),
    .Dim = c(3L, 2L))
}
\seealso{
Other collection_functions: \code{\link{xAsFunction}};
\code{\link{xAssoc}}; \code{\link{xCombos}};
\code{\link{xDiffer}}; \code{\link{xDissoc}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xDrop}}; \code{\link{xExists}};
\code{\link{xFirst}}; \code{\link{xFlatMap}};
\code{\link{xFlatten}}; \code{\link{xFoldListl}};
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
\code{\link{xPartitionWith}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReducer}};
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

Other higher_order_functions: \code{\link{xArity}};
\code{\link{xAsClosure}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}};
\code{\link{xConst}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFlatMap}}; \code{\link{xFlip}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFormals}}; \code{\link{xGet}};
\code{\link{xIterate}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapWhen}}; \code{\link{xMapply}};
\code{\link{xMap}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xPartial}};
\code{\link{xPartitionWith}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xStopWatch}}; \code{\link{xTakeWhile}};
\code{\link{xVectorise}}; \code{\link{xWait}};
\code{\link{xZipWith}}
}

