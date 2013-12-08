\name{xApply}
\alias{xApply}
\title{xApply}
\usage{
  xApply(fn, coll)
}
\arguments{
  \item{fn}{an function of any arity.}

  \item{coll}{a list or pairlist. This may be named, but
  all names must be parameters of \code{fn}.}
}
\value{
  the return value of \code{fn}.
}
\description{
  Call a function with a list of arguments.
}
\section{Uses}{
  \code{xApply} is an adaptor function that allows variadic
  functions to be called with a variable number of
  arguments at runtime. For example, if \code{xConcat} is
  invoked directly it has to be given a fixed number of
  lists to concatenate. If \code{xConcat} is invoked with
  \code{xApply} then xConcat can be invoked with any number
  of lists dynamically.
}
\seealso{
  Other higher_order_functions collection_functions:
  \code{\link{xDo}}, \code{\link{xDropWhile}},
  \code{\link{xExists}}, \code{\link{xFoldl}},
  \code{\link{xFoldListl}}, \code{\link{xFoldr}},
  \code{\link{xForall}}, \code{\link{xLocate}},
  \code{\link{xLocater}}, \code{\link{xPartial}},
  \code{\link{xPartition}}, \code{\link{xPartitionWith}},
  \code{\link{xPermute}}, \code{\link{xPluck}},
  \code{\link{xPoll}}, \code{\link{xReduce}},
  \code{\link{xReducer}}, \code{\link{xReject}},
  \code{\link{xSelect}}, \code{\link{xSplitWith}},
  \code{\link{xTakeWhile}}, \code{\link{xZipWith}}
}

