\name{xDropWhile}
\alias{xDropWhile}
\title{xDropWhile}
\usage{
xDropWhile(pred, coll)
}
\arguments{
  \item{pred}{a unary predicate.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Take every element in a collection from the first time a
predicate is false or na until the end of the collection.
}
\section{Corner Cases}{
  Returns the emty list if \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xDo}}; \code{\link{xExists}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xPartial}}; \code{\link{xPartitionWith}};
\code{\link{xPartition}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xTakeWhile}};
\code{\link{xZipWith}}
}

