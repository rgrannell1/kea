\name{xTakeWhile}
\alias{xTakeWhile}
\title{xTakeWhile}
\usage{
  xTakeWhile(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}
}
\value{
  a list.
}
\description{
  Take every element in a collection from the start until a
  predicate returns false.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero or
  the first element of \code{coll} returns false for the
  predicate. Na values are considered false.
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
  \code{\link{xSplitWith}}, \code{\link{xZipWith}}
}

