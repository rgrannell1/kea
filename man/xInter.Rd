\name{xInter}
\alias{xInter}
\alias{xInter...}
\title{xInter}
\usage{
xInter(colls)

xInter...(...)
}
\arguments{
  \item{colls}{a collection of collections. The collections
  to take the intersection f.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Get the set intersection of several collections.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}; \code{\link{xUnionOf}},
\code{\link{xUnionOf...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

