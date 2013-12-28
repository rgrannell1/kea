\name{xIterate}
\alias{xIterate}
\title{xIterate}
\usage{
xIterate(fn, init)
}
\arguments{
  \item{fn}{a function.}

  \item{init}{an arbitrary value.}
}
\value{
the result of successively applying \code{f} to
\code{init}.
}
\description{
Iteratively apply a function to a value.
}
\section{Corner Cases}{
  length-zero values of \code{init} are handled normally,
  since \code{init} is an arbitrary value. Potentially
  non-terminating.
}
\seealso{
Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
\code{\link{xGet}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapWhen}}; \code{\link{xMapply}};
\code{\link{xMap}}; \code{\link{xNot}};
\code{\link{xParams}}; \code{\link{xPartial}};
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xStopWatch}};
\code{\link{xTakeWhile}}; \code{\link{xVectorise}};
\code{\link{xWait}}; \code{\link{xZipWith}}
}

