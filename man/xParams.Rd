\name{xParams}
\alias{xParams}
\title{xParams}
\usage{
xParams(fn)
}
\arguments{
  \item{fn}{an arbitrary function or primitive function.}
}
\value{
a character vector/.
}
\description{
Get the parametre names of a function or primitive
function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its parametre names, which may not work for all
  functions.
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
\code{\link{xNot}}; \code{\link{xPartial}},
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

Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFlip}};
\code{\link{xFormals}}; \code{\link{xIsVariadic}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

