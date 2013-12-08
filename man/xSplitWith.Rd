\name{xSplitWith}
\alias{xSplitWith}
\title{xSplitWith}
\usage{
  xSplitWith(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}
}
\value{
  a list of two lists.
}
\description{
  Split a list into two lists; all the elements before the
  first time a predicate returns false, and all the
  elements including and after that point.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
  Other higher_order_functions collection_functions:
  \code{\link{xApply}}, \code{\link{xDo}},
  \code{\link{xDropWhile}}, \code{\link{xExists}},
  \code{\link{xFoldl}}, \code{\link{xFoldListl}},
  \code{\link{xFoldr}}, \code{\link{xForall}},
  \code{\link{xLocate}}, \code{\link{xLocater}},
  \code{\link{xPartial}}, \code{\link{xPartition}},
  \code{\link{xPartitionWith}}, \code{\link{xPermute}},
  \code{\link{xPluck}}, \code{\link{xPoll}},
  \code{\link{xReduce}}, \code{\link{xReducer}},
  \code{\link{xReject}}, \code{\link{xSelect}},
  \code{\link{xTakeWhile}}, \code{\link{xZipWith}}
}

