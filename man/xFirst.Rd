\name{xFirst}
\alias{xFirst}
\alias{xFirst...}
\title{xFirst}
\usage{
xFirst(coll)

xFirst...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
the first element in \code{coll}.
}
\description{
Return the first element of a collection.
}
\section{Corner Cases}{
  throws an error if \code{coll} has less than one element;
  this is because any other corner case would violate the
  functions type-signature.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFourth}},
\code{\link{xFourth...}}; \code{\link{xGetKey}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xSecond}}, \code{\link{xSecond...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

