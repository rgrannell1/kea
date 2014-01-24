\name{xTakeWhile}
\alias{xTakeWhile}
\alias{xTakeWhile...}
\title{xTakeWhile}
\usage{
xTakeWhile(pred, coll)

xTakeWhile...(pred, ...)
}
\arguments{
  \item{pred}{a predicate. The functionto test each element
  of the collection with.}

  \item{coll}{a collection. The collection to drop elements
  from.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Take every element in a collection from the start until a
predicate returns false.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero or
  the first element of \bold{coll} returns false for the
  predicate.  Na values are considered false.
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
\code{\link{xTake}}, \code{\link{xTake...}}
}

