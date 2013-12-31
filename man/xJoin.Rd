\name{xJoin}
\alias{xJoin}
\alias{xJoin...}
\title{xJoin}
\usage{
xJoin(colls)

xJoin...(...)
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

Other reshaping_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xFlatten}},
\code{\link{xFlatten...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xRepeat}},
\code{\link{xRepeat...}}; \code{\link{xReverse}};
\code{\link{xSegment}}, \code{\link{xSegment...}};
\code{\link{xShuffle}}, \code{\link{xShuffle...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xSplit}}, \code{\link{xSplit...}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}};
\code{\link{xZip}}, \code{\link{xZip...}}
}

