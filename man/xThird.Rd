\name{xThird}
\alias{xThird}
\title{xThird}
\usage{
xThird(coll)
}
\arguments{
  \item{coll}{a collection}
}
\value{
the third element in \code{coll}.
}
\description{
Return the third value in a collection.
}
\section{Corner Cases}{
  throws an error if \code{coll} has less than three
  elements; this is because any other corner case would
  violate the function's type-signature.
}
\seealso{
Other collection_functions: \code{\link{xApply}};
\code{\link{xAsFunction}}; \code{\link{xAssoc}};
\code{\link{xIsMember}}; \code{\link{xDiffer}};
\code{\link{xDissoc}}; \code{\link{xDrop}};
\code{\link{xFirst}}; \code{\link{xFlatten}};
\code{\link{xFourth}}; \code{\link{xInit}};
\code{\link{xInter}}; \code{\link{xIsEmpty}};
\code{\link{xIsFalse}}; \code{\link{xIsNan}};
\code{\link{xIsNa}}; \code{\link{xIsNull}};
\code{\link{xIsTrue}}; \code{\link{xLast}};
\code{\link{xLength}}; \code{\link{xName}};
\code{\link{xNotFalse}}; \code{\link{xNotNan}};
\code{\link{xNotNa}}; \code{\link{xNotTrue}};
\code{\link{xPack}}; \code{\link{xRepeat}};
\code{\link{xRest}}; \code{\link{xReverse}};
\code{\link{xSecond}}; \code{\link{xSegment}};
\code{\link{xShuffle}}; \code{\link{xSplit}};
\code{\link{xTake}}; \code{\link{xUnion}};
\code{\link{xUnit}}
}

