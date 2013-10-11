\name{xZipWith}
\alias{xZipWith}
\title{Generate a list of n-element lists from n collections, and apply a function to each n-element list.}
\usage{
  xZipWith(fn, ...)
}
\arguments{
  \item{fn}{an n-ary function, or a symbol or name
  identifying such a function.}

  \item{...}{n-vectors, lists or pairlists.}
}
\value{
  returns a list of equal length to the shortest input
  collection.
}
\description{
  Generate a list of n-element lists from n collections,
  and apply a function to each n-element list.
}
\section{Corner Cases}{
  the empty list is returned if the shortest collection has
  length-zero, or no collections are included. Each
  collection is truncated to the length of the shortest
  collection.
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
  \code{\link{xUntil}}, \code{\link{xWait}},
  \code{\link{xWhile}}, \code{\link{xWrap}}
}

