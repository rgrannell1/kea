\name{xSecondOf}
\alias{xSecondOf}
\alias{xSecondOf...}
\title{xSecondOf}
\usage{
xSecondOf(coll)

xSecondOf...(...)
}
\arguments{
  \item{coll}{a collection. The collection to get the
  second element of.}

  \item{...}{see above.}
}
\value{
The second element in \bold{coll}.
}
\description{
Return the second element in a collection.
}
\section{Corner Cases}{
  Throws an error if \bold{coll} has less than two
  elements; this is because any other corner case would
  violate the functions type-signature.
}
\examples{

NULL

}
\seealso{
Other selection_functions: \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirstOf}},
\code{\link{xFirstOf...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xGetKey}};
\code{\link{xInitOf}}, \code{\link{xInitOf...}};
\code{\link{xLastOf}}, \code{\link{xLastOf...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRestOf}}, \code{\link{xRestOf...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

