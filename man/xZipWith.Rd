\name{xZipWith}
\alias{xZipWith}
\title{Generate a list of n-element lists from n collections, and apply a function to each n-element list.}
\usage{
  xZipWith(fn, colls)
}
\arguments{
  \item{fn}{an n-ary function, or a symbol or name
  identifying such a function.}

  \item{colls}{n-vectors, lists or pairlists.}
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
  \code{\link{xSplitWith}}, \code{\link{xTakeWhile}}
}

