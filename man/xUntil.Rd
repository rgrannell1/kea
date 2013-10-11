\name{xUntil}
\alias{xUntil}
\title{Iteratively apply a function until a predicate is met.}
\usage{
  xUntil(pred, fn, init)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{fn}{a function.}

  \item{init}{an arbitrary value.}
}
\value{
  the result of successively applying \code{f} to
  \code{init}.
}
\description{
  Iteratively apply a function until a predicate is met.
}
\section{Corner Cases}{
  length-zero values of \code{init} are handled normally,
  since \code{init} is an arbitrary value. Potentially
  non-terminating.
}
\seealso{
  Other higher_order_function: \code{\link{xAndLift}},
  \code{\link{xApply}}, \code{\link{xArity}},
  \code{\link{xAsUnary}}, \code{\link{xByLift}},
  \code{\link{xChars}}, \code{\link{xCompose}},
  \code{\link{xConst}}, \code{\link{xDifferLift}},
  \code{\link{xDo}}, \code{\link{xDropWhile}},
  \code{\link{xEqualLift}}, \code{\link{xExists}},
  \code{\link{xFixDefs}}, \code{\link{xFlatMap}},
  \code{\link{xFlip}}, \code{\link{xFoldl}},
  \code{\link{xForall}}, \code{\link{xInterLift}},
  \code{\link{xJuxtapose}}, \code{\link{xLimit}},
  \code{\link{xLocate}}, \code{\link{xLocater}},
  \code{\link{xMap}}, \code{\link{xMapAlong}},
  \code{\link{xMapMany}}, \code{\link{xMapWhen}},
  \code{\link{xMinusLift}}, \code{\link{xModLift}},
  \code{\link{xNot}}, \code{\link{xOrLift}},
  \code{\link{xOverLift}}, \code{\link{xParams}},
  \code{\link{xPartMap}}, \code{\link{xPartial}},
  \code{\link{xPartition}}, \code{\link{xPartitionWith}},
  \code{\link{xPluck}}, \code{\link{xPlusLift}},
  \code{\link{xPoll}}, \code{\link{xRecurMap}},
  \code{\link{xReduce}}, \code{\link{xReducer}},
  \code{\link{xReject}}, \code{\link{xScanl}},
  \code{\link{xSelect}}, \code{\link{xSplitWith}},
  \code{\link{xSwap}}, \code{\link{xTakeWhile}},
  \code{\link{xUnfold}}, \code{\link{xUnionLift}},
  \code{\link{xWait}}, \code{\link{xWhile}},
  \code{\link{xWrap}}, \code{\link{xZipWith}}
}

