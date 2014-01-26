\name{xCycle}
\alias{xCycle}
\alias{xCycle...}
\title{xCycle}
\usage{
xCycle(num, coll)

xCycle...(num, ...)
}
\arguments{
  \item{num}{a whole number. The magnitude gives number of
  elements to cycle, and the sign gives the direction:
  positive numbers cause elements from the tail of
  \bold{coll} to be prepended to the output, while negative
  numbers cause elements from the head of \bold{coll} to
  appended to the output.}

  \item{coll}{a collection. The collection to cycle.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Generate a cyclic permutation of a collection.
}
\section{Corner Cases}{
  If \bold{coll} is empty the empty list is returned.
}
\examples{

}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirstOf}},
\code{\link{xFirstOf...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xGetKey}};
\code{\link{xInitOf}}, \code{\link{xInitOf...}};
\code{\link{xLastOf}}, \code{\link{xLastOf...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRestOf}}, \code{\link{xRestOf...}};
\code{\link{xSecondOf}}, \code{\link{xSecondOf...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

