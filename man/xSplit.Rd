\name{xSplit}
\alias{xSplit}
\title{xSplit}
\usage{
  xSplit(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
  a list of two lists; the first list containing the first
  \code{num} elements of \code{coll}, and the second list
  containing the remaining elements \code{coll}.
}
\description{
  Split a collection into elements before and upto an
  index, and after that index.
}
\section{Corner Cases}{
  If \code{num} is zero then the first list in the returned
  value is empty.  Likewise, if \code{num} is equal or
  larger than the length of \code{coll} then the second
  return list is empty. If \code{coll} is length zero both
  lists are empty.
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
  \code{\link{xSecond}}, \code{\link{xSegment}},
  \code{\link{xShuffle}}, \code{\link{xTake}},
  \code{\link{xThird}}, \code{\link{xUnion}},
  \code{\link{xUnit}}
}

