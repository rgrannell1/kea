\name{xTakeWhile}
\alias{xTakeWhile}
\alias{xTakeWhile...}
\title{xTakeWhile}
\usage{
xTakeWhile(pred, coll)

xTakeWhile...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}

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
  returns the empty list if \code{coll} is length-zero or
  the first element of \code{coll} returns false for the
  predicate.  Na values are considered false.
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
\code{\link{xTake}}, \code{\link{xTake...}}
}

