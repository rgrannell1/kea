\name{xTake}
\alias{xTake}
\alias{xTake...}
\title{xTake}
\usage{
xTake(num, coll)

xTake...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{...}{see above.}

  \item{coll}{a collection}
}
\value{
A list.
}
\description{
Take several elements from the head of a collection.
}
\section{Corner Cases}{
  If \code{coll} is empty the empty list is returned.
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
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}}
}

