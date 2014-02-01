\name{xUniqueOf}
\alias{xUniqueOf}
\alias{xUniqueOf...}
\title{xUniqueOf}
\usage{
xUniqueOf(coll)

xUniqueOf...(...)
}
\arguments{
  \item{coll}{a collection. The collection to return unique
  elements from.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Return the unique elements in a collection.
}
\section{Corner Cases}{
  Returns the empty list if \code{colls} is length-zero.
}
\examples{

# 1. Find the smallest unique number in a list.

lowest <-
x_( list(1, 1, 3, 2, 2, 2, 5, 4, 9, 9) ) $
xUniqueOf() $
x_Tap(min \%of\% xAsInteger)
}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}
}

