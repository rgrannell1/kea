\name{xGet}
\alias{xGet}
\title{xGet}
\usage{
xGet(str)
}
\arguments{
  \item{str}{a string.}
}
\value{
a unary function that takes a collection.
}
\description{
Return a function that selects a key from a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
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
\code{\link{xFormals}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xLimit}}; \code{\link{xLocater}},
\code{\link{xLocater...}}; \code{\link{xLocate}},
\code{\link{xLocate...}}, \code{\link{xLocatel}},
\code{\link{xLocatel...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xPartition}};
\code{\link{xPermute}}, \code{\link{xPermute...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xPoll}}, \code{\link{xPoll...}};
\code{\link{xRecurMap}}, \code{\link{xRecurMap...}};
\code{\link{xReducer}}, \code{\link{xReducer...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xSplitWith}}, \code{\link{xSplitWith...}};
\code{\link{xStopwatch}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}; \code{\link{xWait}};
\code{\link{xZipWith}}, \code{\link{xZipWith...}}

Other name_functions: \code{\link{xAssoc}},
\code{\link{xAssoc...}}; \code{\link{xDissoc}},
\code{\link{xDissoc...}}; \code{\link{xName}};
\code{\link{xPluck}}, \code{\link{xPluck...}}

Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFourth}},
\code{\link{xFourth...}}; \code{\link{xInit}},
\code{\link{xInit...}}; \code{\link{xLast}},
\code{\link{xLast...}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xRest}},
\code{\link{xRest...}}; \code{\link{xSecond}},
\code{\link{xSecond...}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xTake}},
\code{\link{xTake...}}
}

