\name{xAt}
\alias{xAt}
\alias{xAt...}
\title{xAt}
\usage{
xAt(num, coll)

xAt...(num, ...)
}
\arguments{
  \item{num}{a whole number. The index to select the
  collection at.  in \bold{coll}.}

  \item{coll}{a collection. The collection to subset.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Select a particular value from a collection.
}
\details{
\bold{xAt} is similar to base R's subsetting operator '[[',
except that it performs more validation on the input
indices, and it acts as a normal function.
}
\section{Corner Cases}{
  \bold{xAt} does not allow subscripting of values out of
  bounds.  If an index larger than the maximum value in
  \bold{coll} is given an error is thrown.
}
\examples{

NULL
}
\seealso{
Other selection_functions: \code{\link{xAtCol}},
\code{\link{xAtCol...}}; \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirstOf}},
\code{\link{xFirstOf...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xInitOf}},
\code{\link{xInitOf...}}; \code{\link{xLastOf}},
\code{\link{xLastOf...}}; \code{\link{xPluck}},
\code{\link{xPluck...}}; \code{\link{xRestOf}},
\code{\link{xRestOf...}}; \code{\link{xSecondOf}},
\code{\link{xSecondOf...}}; \code{\link{xSlice}},
\code{\link{xSlice...}}; \code{\link{xTakeWhile}},
\code{\link{xTakeWhile...}}; \code{\link{xTake}},
\code{\link{xTake...}}; \code{\link{xThirdOf}},
\code{\link{xThirdOf...}}
}

