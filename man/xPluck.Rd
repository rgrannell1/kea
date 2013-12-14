\name{xPluck}
\alias{xPluck}
\title{xPluck}
\usage{
xPluck(str, coll)
}
\arguments{
  \item{str}{a string.}

  \item{coll}{a list or pairlist of lists or pairlists.}
}
\value{
a list.
}
\description{
Map over a collection of lists or pairlists, selecting
fields in each element by name.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.  If
  str is length-zero then the empty string "" is used to
  match key-names.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xLocater}};
\code{\link{xLocate}}; \code{\link{xPartial}};
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPoll}};
\code{\link{xReducer}}; \code{\link{xReduce}};
\code{\link{xReject}}; \code{\link{xSelect}};
\code{\link{xSplitWith}}; \code{\link{xTakeWhile}};
\code{\link{xZipWith}}
}

