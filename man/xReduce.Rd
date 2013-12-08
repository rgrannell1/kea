\name{xReduce}
\alias{xReduce}
\title{xReduce}
\usage{
  xReduce(fn, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{coll}{a collection.}
}
\value{
  an arbitrary value, depending on the function \code{fn}.
}
\description{
  Fold a function over a collection from left to right.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero, and
  returns the value inside \code{coll} if coll is
  length-one.
}

\section{Short-Circuiting}{
  The fold family of functions include an optional
  mechanism of prematurely returning an answer, similar to
  the \code{return()} function in base R. By calling
  \code{Return} with the value you want \code{fn} to yield
  instantly, the fold/reduce is halted, and that value is
  yielded as the result of that proceedure. This allows
  fold/reduce to have sub-O(n) efficiency.

  \code{Return()} essentially acts as a higher-order return
  function, that breaks "further" than the normal return
  statement.
}
\seealso{
  Other higher_order_functions collection_functions:
  \code{\link{xApply}}, \code{\link{xDo}},
  \code{\link{xDropWhile}}, \code{\link{xExists}},
  \code{\link{xFoldl}}, \code{\link{xFoldListl}},
  \code{\link{xFoldr}}, \code{\link{xForall}},
  \code{\link{xLocate}}, \code{\link{xLocater}},
  \code{\link{xPartial}}, \code{\link{xPartition}},
  \code{\link{xPartitionWith}}, \code{\link{xPermute}},
  \code{\link{xPluck}}, \code{\link{xPoll}},
  \code{\link{xReducer}}, \code{\link{xReject}},
  \code{\link{xSelect}}, \code{\link{xSplitWith}},
  \code{\link{xTakeWhile}}, \code{\link{xZipWith}}
}

