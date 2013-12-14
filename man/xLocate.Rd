\name{xLocate}
\alias{xLocate}
\title{xLocate}
\usage{
xLocate(pred, coll)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{coll}{a collection.}
}
\value{
an length-one or length-zero whole number.
}
\description{
Get the position of the first element for which a predicate
returns true.
}
\section{Corner Cases}{
  returns integer(0) if no match is found.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xLocater}};
\code{\link{xPartial}}; \code{\link{xPartitionWith}};
\code{\link{xPartition}}; \code{\link{xPermute}};
\code{\link{xPluck}}; \code{\link{xPoll}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xTakeWhile}};
\code{\link{xZipWith}}
}

