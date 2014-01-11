\name{xFourth}
\alias{xFourth}
\alias{xFourth...}
\title{xFourth}
\usage{
xFourth(coll)

xFourth...(...)
}
\arguments{
  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
The fourth value in \code{coll}.
}
\description{
Return the fourth value in a collection.
}
\section{Corner Cases}{
  throws an error if \code{coll} has less than four
  element; this is because any other corner case would
  violate the functions type-signature.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xGetKey}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xSecond}}, \code{\link{xSecond...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

