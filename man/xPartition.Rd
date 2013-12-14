\name{xPartition}
\alias{xPartition}
\title{xPartition}
\usage{
xPartition(pred, coll)
}
\arguments{
  \item{pred}{a binary predicate function.}

  \item{coll}{a collection}
}
\value{
a list of lists.
}
\description{
Divide elements in a collection into lists based on an
equivelence predicate.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xApply}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xPartial}}; \code{\link{xPartitionWith}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xTakeWhile}}; \code{\link{xZipWith}}
}

