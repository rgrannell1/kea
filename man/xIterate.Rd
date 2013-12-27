\name{xIterate}
\alias{xIterate}
\title{Iteratively apply a function to a value.}
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
\code{\link{xArity}}; \code{\link{xAsUnary}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xLimit}}; \code{\link{xLocate}};
\code{\link{xLocater}}; \code{\link{xMap}};
\code{\link{xMapIndexed}}; \code{\link{xMapMany}};
\code{\link{xMapply}}; \code{\link{xMapWhen}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xPartial}}; \code{\link{xPartition}};
\code{\link{xPartitionWith}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xRecurMap}}; \code{\link{xReduce}};
\code{\link{xReducer}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xStopWatch}}; \code{\link{xTakeWhile}};
\code{\link{xVectorise}}; \code{\link{xWait}};
\code{\link{xZipWith}}
}

