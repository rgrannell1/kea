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
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL

}
\seealso{
Other name_functions: \code{\link{xAsNamed}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xUnzipNames}}, \code{\link{xUnzipNames...}};
\code{\link{xZipNames}}, \code{\link{xZipNames...}}

Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirstOf}},
\code{\link{xFirstOf...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xInitOf}},
\code{\link{xInitOf...}}; \code{\link{xLastOf}},
\code{\link{xLastOf...}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xRestOf}},
\code{\link{xRestOf...}}; \code{\link{xSecondOf}},
\code{\link{xSecondOf...}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xTake}},
\code{\link{xTake...}}
}

