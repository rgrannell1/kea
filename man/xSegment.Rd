\name{xSegment}
\alias{xSegment}
\title{xSegment}
\usage{
  xSegment(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
  a list of n-element lists.
}
\description{
  Divide a collection into segments of fixed length.
}
\section{Uses}{
  \code{xSegment} is useful for reshaping a collection into
  pairs, triples, or larger groups before applying a
  function to each group.
}

\section{Corner Cases}{
  the final list in the return value will have less than
  \code{num} elements if \code{length(coll)} is not evenly
  divisible by \code{num}.  if \code{coll} is length-zero,
  the empty list is returned.
}
\seealso{
  Other collection_functions: \code{\link{xAsFunction}},
  \code{\link{xAssoc}}, \code{\link{xContains}},
  \code{\link{xDiffer}}, \code{\link{xDissoc}},
  \code{\link{xDrop}}, \code{\link{xFirst}},
  \code{\link{xFlatten}}, \code{\link{xFourth}},
  \code{\link{xInit}}, \code{\link{xInter}},
  \code{\link{xIsEmpty}}, \code{\link{xIsFalse}},
  \code{\link{xIsNa}}, \code{\link{xIsNan}},
  \code{\link{xIsNull}}, \code{\link{xIsTrue}},
  \code{\link{xLast}}, \code{\link{xLength}},
  \code{\link{xName}}, \code{\link{xNotFalse}},
  \code{\link{xNotNa}}, \code{\link{xNotNan}},
  \code{\link{xNotTrue}}, \code{\link{xPack}},
  \code{\link{xRepeat}}, \code{\link{xRest}},
  \code{\link{xSecond}}, \code{\link{xShuffle}},
  \code{\link{xSplit}}, \code{\link{xTake}},
  \code{\link{xThird}}, \code{\link{xUnion}},
  \code{\link{xUnit}}
}

