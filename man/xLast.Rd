\name{xLast}
\alias{xLast}
\alias{xLast...}
\title{xLast}
\usage{
xLast(coll)

xLast...(...)
}
\arguments{
  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
The value of the last element in \code{coll}.
}
\description{
Return the last element in a collection.
}
\section{Corner Cases}{
  Throws an error if \code{coll} has less than one element;
  this is because any other corner case would violate the
  function's type-signature.
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
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xSecond}}, \code{\link{xSecond...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

