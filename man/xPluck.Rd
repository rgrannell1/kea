\name{xPluck}
\alias{xPluck}
\alias{xPluck...}
\title{xPluck}
\usage{
xPluck(str, colls)

xPluck...(str, ...)
}
\arguments{
  \item{str}{a string.}

  \item{colls}{a list or pairlist of lists or pairlists.}

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
  Returns the empty list if \code{colls} is length-zero.
  If str is length-zero then the empty string "" is used to
  match key-names.
}
\seealso{
Other name_functions: \code{\link{xAsNamed}};
\code{\link{xAssoc}}, \code{\link{xAssoc...}};
\code{\link{xDissoc}}, \code{\link{xDissoc...}};
\code{\link{xGetKey}}

Other selection_functions: \code{\link{xDropWhile}},
\code{\link{xDropWhile...}}; \code{\link{xDrop}},
\code{\link{xDrop...}}; \code{\link{xFirstOf}},
\code{\link{xFirstOf...}}; \code{\link{xFourthOf}},
\code{\link{xFourthOf...}}; \code{\link{xGetKey}};
\code{\link{xInitOf}}, \code{\link{xInitOf...}};
\code{\link{xLastOf}}, \code{\link{xLastOf...}};
\code{\link{xRestOf}}, \code{\link{xRestOf...}};
\code{\link{xSecondOf}}, \code{\link{xSecondOf...}};
\code{\link{xTakeWhile}}, \code{\link{xTakeWhile...}};
\code{\link{xTake}}, \code{\link{xTake...}}
}

