\name{xReduce}
\alias{xReduce}
\alias{xReduce...}
\alias{xReducel}
\alias{xReducel...}
\title{xReduce}
\usage{
xReduce(fn, coll)

xReducel(fn, coll)

xReducel...(fn, ...)

xReduce...(fn, ...)
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

\section{Variadic functions}{
  Many arrow functions come in two forms:

  \itemize{ \item{\bold{xMethod:}}{ These functions do not
  collect arguments with the '...' parametre.}
  \item{\bold{xMethod...:}}{ These functions collect
  arguments with the '...' parametre} }

  The xMethod... form is somewhat shorter to write, but it
  is mainly used when the arrow function will take a fixed
  number of arguments. An ideal use-case would be writing

  \code{xMap...( xGetKey('a'),\cr list(a = 1, b = 1),\cr
  list(a = 2, b = 1))}

  instead of

  \code{xMap.( xGetKey('a'),\cr list(\cr list(a = 1, b =
  1),\cr list(a = 2, b = 1)) )}

  For other applications the xMethod form of the function
  is more likely to be useful:

  \code{nums <- (1:10) / 10}

  \code{xMap(sqrt, nums)}

  As opposed to

  \code{xApply(xMap..., list(sqrt, nums))}

  Arrow methods also have variadic and non-variadic forms
  in many cases.
}

\section{Short-Circuiting}{
  The fold family of functions include an optional
  mechanism of prematurely returning an answer, similar to
  the \code{return()} function in base R. By calling
  \code{Return} with the value you want \code{fn} to yield
  instantly, the fold/reduce is halted, and that value is
  yielded as the result of that proceedure.

  \code{Return()} essentially acts as a higher-order return
  function, that breaks "further" than the normal return
  statement.

  \code{to_search <- c(list(0, 1, 0, 3, 2, 10), rep(4,
  1000))}

  \code{ xFoldl(\cr (checked : searched ) := {\cr if
  (searched == 10) Return(checked) else checked + 1\cr
  },\cr list(),\cr to_search\cr ) }

  \code{6}

  In this case, the Foldl returns its results prematurely
  after 6 searches, as opposed to the thousands of
  iterations that the base function \code{Reduce()} would
  have to preform before returning its result. By using
  \code{Return()} where possible folding functions can be
  made to preform significantly bettern
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
    c(3, 4)
)

matrix(1:4, 2, 2)
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
\code{\link{xFirst...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xFlatten}},
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

Other folding_functions: \code{\link{xFoldList}},
\code{\link{xFoldList...}}, \code{\link{xFoldListl}},
\code{\link{xFoldListl...}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}, \code{\link{xFoldr...}};
\code{\link{xReducer}}, \code{\link{xReducer...}}

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
\code{\link{xFlatMap}}, \code{\link{xFlatMap...}};
\code{\link{xFlip}}; \code{\link{xFoldList}},
\code{\link{xFoldList...}}, \code{\link{xFoldListl}},
\code{\link{xFoldListl...}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}, \code{\link{xFoldr...}};
\code{\link{xForall}}, \code{\link{xForall...}};
\code{\link{xFormals}}; \code{\link{xGetKey}};
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
\code{\link{xReducer...}}; \code{\link{xReject}},
\code{\link{xReject...}}; \code{\link{xSplitWith}},
\code{\link{xSplitWith...}}; \code{\link{xStopwatch}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}};
\code{\link{xWait}}; \code{\link{xZipWith}},
\code{\link{xZipWith...}}
}

