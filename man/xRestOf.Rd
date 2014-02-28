\name{xRestOf}
\alias{xRestOf}
\alias{xRestOf...}
\title{xRestOf}
\usage{
xRestOf(coll)

xRestOf...(...)
}
\arguments{
  \item{coll}{a collection. The collection to subset.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Remove the first element from a collection.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL

}
\seealso{
Other selection_functions: \code{\link{xAtCol}},
\code{\link{xAtCol...}}; \code{\link{xAt}},
\code{\link{xAt...}}; \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirstOf}},
\code{\link{xFirstOf...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xGetKey}};
\code{\link{xInitOf}}, \code{\link{xInitOf...}};
\code{\link{xLastOf}}, \code{\link{xLastOf...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xSecondOf}}, \code{\link{xSecondOf...}};
\code{\link{xSlice}}, \code{\link{xSlice...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}};
\code{\link{xThirdOf}}, \code{\link{xThirdOf...}}
}

