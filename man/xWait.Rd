\name{xWait}
\alias{xWait}
\title{xWait}
\usage{
xWait(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{num}{a nonnegative whole number.}
}
\value{
a function with the same parametres as \code{fn}.
}
\description{
Delay the evalution of a function after invokation for a
set amount of time.
}
\section{Corner Cases}{
  if \code{num} is zero then \code{fn} is returned
  untouched.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xNot}};
\code{\link{xPartial}}; \code{\link{xThread}};
\code{\link{xVectorise}}

Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFlatMap}};
\code{\link{xFlip}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xFormals}};
\code{\link{xGet}}; \code{\link{xIterate}};
\code{\link{xJuxtapose}}; \code{\link{xLift}};
\code{\link{xLimit}}; \code{\link{xLocater}};
\code{\link{xLocate}}; \code{\link{xMapIndexed}};
\code{\link{xMapMany}}; \code{\link{xMapWhen}};
\code{\link{xMapply}}; \code{\link{xMap}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xPartial}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xRecurMap}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xStopwatch}};
\code{\link{xTakeWhile}}; \code{\link{xVectorise}};
\code{\link{xZipWith}}

Other time_functions: \code{\link{xLimit}};
\code{\link{xStopwatch}}
}

