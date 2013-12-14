\name{xFoldListl}
\alias{xFoldListl}
\title{xFoldListl}
\usage{
xFoldListl(fn, init, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its right argument.}

  \item{init}{an arbitrary value.}

  \item{coll}{a collection.}
}
\value{
a list with its init element being \code{coll}, and
containing \code{length(coll) + 1}.
}
\description{
Fold a function over a collection from left to right with
an initital left value, keeping intermediate values.
}
\section{Corner Cases}{
  returns \code{list(init)} if \code{coll} is length-zero.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xApply}}; \code{\link{xDo}};
\code{\link{xDropWhile}}; \code{\link{xExists}};
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

