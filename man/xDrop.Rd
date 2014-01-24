\name{xDrop}
\alias{xDrop}
\alias{xDrop...}
\title{xDrop}
\usage{
xDrop(num, coll)

xDrop...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number. The number of
  elements to return from the input collection.}

  \item{coll}{a collection. The collection to subset.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Take several elements from the head of a collection.
}
\section{Corner Cases}{
  If \bold{coll} is empty the empty list is returned.
}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xFirstOf}},
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

