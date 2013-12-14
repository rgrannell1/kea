\name{xPartitionWith}
\alias{xPartitionWith}
\title{xPartitionWith}
\usage{
xPartitionWith(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}
}
\value{
Returns two lists; a list of elements in a collection for
which a predicate returns true, and a list of elements in a
collection for which a predicate returns false or na.
}
\description{
Divide elements in a collection into two collections based
on a predicate function.
}
\section{Corner Cases}{
  if \code{coll} is empty the empty list is returned. If
  all the elements return only true/only false, then one of
  two sublists will be the empty list.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xApply}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xPartial}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xTakeWhile}}; \code{\link{xZipWith}}
}

