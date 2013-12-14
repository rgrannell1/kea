\name{xLocater}
\alias{xLocater}
\title{xLocater}
\usage{
xLocater(pred, coll)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{coll}{a collection.}
}
\value{
an integer.
}
\description{
Get the position of the last element for which a predicate
returns true.
}
\section{Corner Cases}{
  returns integer(0) if no match is found.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xApply}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xLocate}}; \code{\link{xPartial}};
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xTakeWhile}}; \code{\link{xZipWith}}
}

