\name{xUnion}
\alias{xUnion}
\alias{xUnion...}
\title{xUnion
``
Get the set union of several collections.}
\usage{
xUnion(colls)

xUnion...(...)
}
\arguments{
  \item{colls}{a collection of collections.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
xUnion `` Get the set union of several collections.
}
\details{
\code{xUnion} returns a list of the unique elements in the
combination of each collection in \code{colls}.
}
\section{Corner Cases}{
  Returns the empty list if \code{colls} is length-zero.
}
\examples{

NULL

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

