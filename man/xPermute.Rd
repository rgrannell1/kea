\name{xPermute}
\alias{xPermute}
\title{xPermute}
\usage{
xPermute(coll, colls)
}
\arguments{
  \item{coll}{a collection of whole numbers.}

  \item{colls}{several collections of equal length to
  \code{coll}.}
}
\value{
a list of collections.
}
\description{
Permute several collections using indices.
}
\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xApply}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
\code{\link{xFoldListl}}; \code{\link{xFoldl}};
\code{\link{xFoldr}}; \code{\link{xForall}};
\code{\link{xLocater}}; \code{\link{xLocate}};
\code{\link{xPartial}}; \code{\link{xPartitionWith}};
\code{\link{xPartition}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSelect}}; \code{\link{xSplitWith}};
\code{\link{xTakeWhile}}; \code{\link{xZipWith}}
}

