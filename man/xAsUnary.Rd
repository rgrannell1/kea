\name{xAsUnary}
\alias{xAsUnary}
\title{xAsUnary}
\usage{
xAsUnary(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
a unary function of x.
}
\description{
Create a function that takes one argument, and applies each
element of that argument as a parametre of its underlying
function.
}
\examples{

# 1. xAsUnary can be used to adapt higher-order
#    functions to make them work with non-unary functions.

xSelect...(
    xAsUnary(
        (a : b) := {
            a > 0 && b >= 0
        }
    ),
    list(+1, +1),
    list(+0, +1),
    list(-1, 1),
    list(-1, -1)
)

list(+1, +1)
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}};
\code{\link{xConst}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xNot}}; \code{\link{xPartial}};
\code{\link{xThread}}; \code{\link{xVectorise}};
\code{\link{xWait}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}};
\code{\link{xConst}}; \code{\link{xDo}};
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

Other parametre_functions: \code{\link{xAsVariadic}};
\code{\link{xFlip}}; \code{\link{xFormals}};
\code{\link{xIsVariadic}}; \code{\link{xParams}};
\code{\link{xPartial}}
}

