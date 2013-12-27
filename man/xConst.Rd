\name{xConst}
\alias{xConst}
\title{xConst}
\usage{
xConst(val)
}
\arguments{
  \item{val}{an arbitrary value.}
}
\value{
a variadic function.
}
\description{
Create a function that returns a particular value.
}
\section{Uses}{
  \code{xConst} is useful when used alongside the lift
  family of functions. For example, the \code{xModLift}
  function could be partially applied with \code{xConst(6)}
  as its left argument to create a \code{xModLift6}
  function, that modulo-6s the output of its underlying
  function. \code{xConst} is also useful for creating
  function analogues to TRUE, FALSE and NA for use with
  arrow functions that take a predicate.
}
\seealso{
Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsUnary}};
\code{\link{xCompose}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFlatMap}}; \code{\link{xFlip}};
\code{\link{xFoldl}}; \code{\link{xFoldListl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xFormals}}; \code{\link{xIterate}};
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

