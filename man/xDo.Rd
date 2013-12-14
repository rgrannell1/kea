\name{xDo}
\alias{xDo}
\title{xDo}
\usage{
xDo(fn, coll)
}
\arguments{
  \item{fn}{a unary function, usually side-effectful.}

  \item{coll}{a collection}
}
\value{
a list.
}
\description{
Map (a possibly side-effectful) function over a collection
and discard the results.
}
\section{Uses}{
  \code{xDo} behaves similarily to \code{xMap}; it applies
  a function to every element of a collection. However,
  \code{xDo} is primarily meant for use with side-effectful
  functions, and is more memory efficient for \code{xDo}
  for this task. \code{xDo} can be used for plotting every
  point in a list of xy coordinates, or for printing every
  value in a list. It is also possible to use \code{xDo}
  for side-effectfully updating values, but this can lead
  to stateful, tangled code.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xDropWhile}}; \code{\link{xExists}};
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

