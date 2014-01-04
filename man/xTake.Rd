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
a list.
}
\description{
Take several elements from the head of a collection.
}
\section{Corner Cases}{
  If \code{coll} is empty the empty list is returned.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFourth}},
\code{\link{xFourth...}}; \code{\link{xGetKey}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xSecond}}, \code{\link{xSecond...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}}
}

