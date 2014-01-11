\name{xPluck}
\alias{xPluck}
\alias{xPluck...}
\title{xPluck}
\usage{
xPluck(str, coll)

xPluck...(str, ...)
}
\arguments{
  \item{str}{a string.}

  \item{coll}{a list or pairlist of lists or pairlists.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Map over a collection of lists or pairlists, selecting
fields in each element by name.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.  If
  str is length-zero then the empty string "" is used to
  match key-names.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other name_functions: \code{\link{xAsNamed}};
\code{\link{xAssoc}}, \code{\link{xAssoc...}};
\code{\link{xDissoc}}, \code{\link{xDissoc...}};
\code{\link{xGetKey}}

Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirst}},
\code{\link{xFirst...}}; \code{\link{xFourth}},
\code{\link{xFourth...}}; \code{\link{xGetKey}};
\code{\link{xInit}}, \code{\link{xInit...}};
\code{\link{xLast}}, \code{\link{xLast...}};
\code{\link{xRest}}, \code{\link{xRest...}};
\code{\link{xSecond}}, \code{\link{xSecond...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

