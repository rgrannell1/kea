\name{xSecond}
\alias{xSecond}
\alias{xSecond...}
\title{xSecond}
\usage{
xSecond(coll)

xSecond...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
The second element in \code{coll}.
}
\description{
Return the second element in a collection.
}
\section{Corner Cases}{
  Throws an error if \code{coll} has less than two
  elements; this is because any other corner case would
  violate the functions type-signature.
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
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

