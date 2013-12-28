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
Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldl}};
\code{\link{xFoldListl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
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

Other name_functions: \code{\link{xAssoc}};
\code{\link{xDissoc}}; \code{\link{xName}};
\code{\link{xPluck}}

Other selection_functions: \code{\link{xDrop}};
\code{\link{xDropWhile}}; \code{\link{xFirst}};
\code{\link{xFourth}}; \code{\link{xInit}};
\code{\link{xLast}}; \code{\link{xPluck}};
\code{\link{xRest}}; \code{\link{xSecond}};
\code{\link{xTake}}; \code{\link{xTakeWhile}}
}

