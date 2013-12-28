\name{xAsVariadic}
\alias{xAsVariadic}
\title{xAsVariadic}
\usage{
xAsVariadic(fn)
}
\arguments{
  \item{fn}{a unary function.}
}
\value{
a variadic function.
}
\description{
Return a variadic function that passes its arguments as a
list to ints underyling function.
}
\examples{

# xAsVariadic lets you take a non-unary function and
# creates a function that lets you pass it arguments
# as a single list.
#

# 1. create a version of sum that
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}};
\code{\link{xAsUnary}}; \code{\link{xCompose}};
\code{\link{xConst}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xNot}}; \code{\link{xPartial}};
\code{\link{xThread}}; \code{\link{xVectorise}};
\code{\link{xWait}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xCompose}};
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

Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xFlip}}; \code{\link{xFormals}};
\code{\link{xIsVariadic}}; \code{\link{xParams}};
\code{\link{xPartial}}
}

