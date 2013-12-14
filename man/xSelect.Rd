\name{xSelect}
\alias{xSelect}
\title{xSelect}
\usage{
xSelect(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}
}
\value{
a list.
}
\description{
Include all elements from a collection matching a
predicate.
}
\section{Uses}{
  The select function is useful for taking a collection,
  and returning values that meet certain criteria. Likely
  uses include selecting rows in a (converted) data frame
  that contain a certain value, selecting strings that
  match a regular expression, or selecting records based on
  a value in a particular field.
}

\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero, or
  no match is found.
}
\seealso{
Other higher_order_functions collection_functions:
\code{\link{xDo}}; \code{\link{xDropWhile}};
\code{\link{xExists}}; \code{\link{xFoldListl}};
\code{\link{xFoldl}}; \code{\link{xFoldr}};
\code{\link{xForall}}; \code{\link{xLocater}};
\code{\link{xLocate}}; \code{\link{xPartial}};
\code{\link{xPartitionWith}}; \code{\link{xPartition}};
\code{\link{xPermute}}; \code{\link{xPluck}};
\code{\link{xPoll}}; \code{\link{xReducer}};
\code{\link{xReduce}}; \code{\link{xReject}};
\code{\link{xSplitWith}}; \code{\link{xTakeWhile}};
\code{\link{xZipWith}}
}

