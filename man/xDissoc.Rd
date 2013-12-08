\name{xDissoc}
\alias{xDissoc}
\title{xDissoc}
\usage{
  xDissoc(coll)
}
\arguments{
  \item{coll}{a list or pairlist of list or pairlist pairs,
  with the first element being a string and the second
  element being any value.}
}
\value{
  a named list.
}
\description{
  Split a named list into a list of name: value lists.
}
\section{Corner Cases}{
  returns \code{list()} if \code{coll} is length-zero.
}
\seealso{
  Other collection_functions: \code{\link{xAsFunction}},
  \code{\link{xAssoc}}, \code{\link{xContains}},
  \code{\link{xDiffer}}, \code{\link{xDrop}},
  \code{\link{xFirst}}, \code{\link{xFlatten}},
  \code{\link{xFourth}}, \code{\link{xInit}},
  \code{\link{xInter}}, \code{\link{xIsEmpty}},
  \code{\link{xIsFalse}}, \code{\link{xIsNa}},
  \code{\link{xIsNan}}, \code{\link{xIsNull}},
  \code{\link{xIsTrue}}, \code{\link{xLast}},
  \code{\link{xLength}}, \code{\link{xName}},
  \code{\link{xNotFalse}}, \code{\link{xNotNa}},
  \code{\link{xNotNan}}, \code{\link{xNotTrue}},
  \code{\link{xPack}}, \code{\link{xRepeat}},
  \code{\link{xRest}}, \code{\link{xSecond}},
  \code{\link{xSegment}}, \code{\link{xShuffle}},
  \code{\link{xSplit}}, \code{\link{xTake}},
  \code{\link{xThird}}, \code{\link{xUnion}},
  \code{\link{xUnit}}
}

