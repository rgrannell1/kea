\name{xDrop}
\alias{xDrop}
\alias{xDrop...}
\title{xDrop}
\usage{
xDrop(num, coll)

xDrop...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection.}

  \item{...}{see above.}
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

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFourth}},
\code{\link{xFourth...}}; \code{\link{xGetKey}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xSecond}}, \code{\link{xSecond...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

