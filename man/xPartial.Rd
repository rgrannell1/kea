\name{xPartial}
\alias{xPartial}
\title{xPartial}
\usage{
  xPartial(fn, coll)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{coll}{a collection}
}
\value{
  A function of equal or lesser arity to \code{fn}.
}
\description{
  Partially apply a function.
}
\section{Corner Cases}{
  Partial application also works for ellipses (eg list(...
  = 1)).
}
\seealso{
  Other higher_order_functions collection_functions:
  \code{\link{xApply}}, \code{\link{xDo}},
  \code{\link{xDropWhile}}, \code{\link{xExists}},
  \code{\link{xFoldl}}, \code{\link{xFoldListl}},
  \code{\link{xFoldr}}, \code{\link{xForall}},
  \code{\link{xLocate}}, \code{\link{xLocater}},
  \code{\link{xPartition}}, \code{\link{xPartitionWith}},
  \code{\link{xPermute}}, \code{\link{xPluck}},
  \code{\link{xPoll}}, \code{\link{xReduce}},
  \code{\link{xReducer}}, \code{\link{xReject}},
  \code{\link{xSelect}}, \code{\link{xSplitWith}},
  \code{\link{xTakeWhile}}, \code{\link{xZipWith}}
}

