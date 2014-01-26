\name{xCycle}
\alias{xCycle}
\alias{xCycle...}
\title{xCycle}
\usage{
xCycle(num, coll)

xCycle...(num, ...)
}
\arguments{
  \item{num}{a whole number. The magnitude gives number of
  elements to cycle, and the sign gives the direction:
  positive numbers cause elements from the tail of
  \bold{coll} to be prepended to the output, while negative
  numbers cause elements from the head of \bold{coll} to
  appended to the output.}

  \item{coll}{a collection. The collection to cycle.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Generate a cyclic permutation of a collection.
}
\section{Corner Cases}{
  If \bold{coll} is empty the empty list is returned.
}
\examples{

}
\seealso{
Other combinatoric_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}

Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDuplicated}},
\code{\link{xDuplicated...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUnique}},
\code{\link{xUnique...}}
}
