\name{xExists}
\alias{xExists}
\title{xExists}
\usage{
xExists(pred, colls)
}
\arguments{
  \item{pred}{an n-ary predicate.}

  \item{colls}{n collections.}
}
\value{
a boolean value.
}
\description{
Does any selection of arguments satisfy a predicate?
}
\section{Corner Cases}{
  if any collection is length zero (or no collections are
  given), then False is returned.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xApply}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xLocater}};
\code{\link{xLocate}}; \code{\link{xPartial}};
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xTakeWhile}}; \code{\link{xZipWith}}
}

