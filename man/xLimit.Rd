\name{xLimit}
\alias{xLimit}
\title{xLimit}
\usage{
xLimit(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{num}{a positive whole number.}
}
\value{
A function with the same parametres as \code{fn}.
}
\description{
Create a function that can call its underlying function a
limited number of times.
}
\section{Uses}{
  \code{xLimit} returns a function that behaves identically
  to the original input function, but has a limited
  lifespan. A natural application of \code{xLimit} is for
  creating functions that interact with web API's; most web
  API's only allow users to make a preset number of
  requests per hour. \code{xLimit} could be used to create
  functions with a built-in rate limit, to avoid having to
  writing rate-limit checking boilerplate.
}
\seealso{
Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
\code{\link{xGet}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
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

Other time_functions: \code{\link{xStopWatch}};
\code{\link{xWait}}
}

