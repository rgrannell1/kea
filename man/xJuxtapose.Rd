\name{xJuxtapose}
\alias{xJuxtapose}
\alias{xJuxtapose...}
\title{xJuxtapose}
\usage{
xJuxtapose(fns)

xJuxtapose...(...)
}
\arguments{
  \item{fns}{a list or pairlist of functions.}
}
\value{
a variadic function.
}
\description{
Create a function that applies an argument to a list of
underlying functions.
}
\section{Corner Cases}{
  If no functions are provided the empty list is returned.
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
Other function_combining_functions: \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xLift}},
\code{\link{xLift...}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xFlip}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}};
\code{\link{xWait}}

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
\code{\link{xIterate}}; \code{\link{xLift}},
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
}

