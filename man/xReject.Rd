\name{xReject}
\alias{xReject}
\title{xReject}
\usage{
  xReject(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}
}
\value{
  a list.
}
\description{
  Remove all elements from a collection matching a
  predicate.
}
\section{Uses}{
  The uses for the reject function are very similar to
  those for select.
}

\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
  Throws an error if pred returns a non-boolean value.
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
  \code{\link{xSelect}}, \code{\link{xSplitWith}},
  \code{\link{xTakeWhile}}, \code{\link{xZipWith}}
}

