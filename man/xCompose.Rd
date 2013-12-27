\name{xCompose}
\alias{xCompose}
\title{xCompose}
\usage{
xCompose(fns)
}
\arguments{
  \item{fn1}{a unary function.}

  \item{fn2}{a unary function.}
}
\value{
A function with the same parametres as \code{fn2}.
}
\description{
Compose two functions.
}
\details{
Unlike some other functions that return functions in Arrow,
\code{xCompose} preserves the parametres of one of its
input functions (the right function).  This is not always
possible for some other functions, but it is included in
\code{xCompose} as the function is ubiquitous and should be
as powerful as possible, even at the risk of making Arrow
less consistent.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xConst}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xNot}}; \code{\link{xPartial}};
\code{\link{xThread}}; \code{\link{xVectorise}};
\code{\link{xWait}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
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
}

