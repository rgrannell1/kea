\name{xGetKey}
\alias{xGetKey}
\title{xGetKey}
\usage{
xGetKey(str)
}
\arguments{
  \item{str}{a string.}
}
\value{
A unary function that takes a collection.
}
\description{
Return a function that selects a key from a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other name_functions: \code{\link{xAsNamed}};
\code{\link{xAssoc}}, \code{\link{xAssoc...}};
\code{\link{xDissoc}}, \code{\link{xDissoc...}};
\code{\link{xPluck}}, \code{\link{xPluck...}}

Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFourth}},
\code{\link{xFourth...}}; \code{\link{xInit}},
\code{\link{xInit...}}; \code{\link{xLast}},
\code{\link{xLast...}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xRest}},
\code{\link{xRest...}}; \code{\link{xSecond}},
\code{\link{xSecond...}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xTake}},
\code{\link{xTake...}}
}

