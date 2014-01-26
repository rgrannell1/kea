\name{xFirstOf}
\alias{xFirstOf}
\alias{xFirstOf...}
\title{xFirstOf}
\usage{
xFirstOf(coll)

xFirstOf...(...)
}
\arguments{
  \item{coll}{a collection. The collection to get the first
  element of.}

  \item{...}{see above.}
}
\value{
The first element in \bold{coll}.
}
\description{
Return the first element of a collection.
}
\section{Corner Cases}{
  Throws an error if \bold{coll} has less than one element;
  this is because any other corner case would violate the
  functions type-signature.
}
\examples{

NULL

}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xGetKey}};
\code{\link{xInitOf}}, \code{\link{xInitOf...}};
\code{\link{xLastOf}}, \code{\link{xLastOf...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRestOf}}, \code{\link{xRestOf...}};
\code{\link{xSecondOf}}, \code{\link{xSecondOf...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

