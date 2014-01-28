\name{xIsMember}
\alias{xIsMember}
\alias{xIsMember...}
\title{xIsMember}
\usage{
xIsMember(val, coll)

xIsMember...(val, ...)
}
\arguments{
  \item{val}{an arbitrary value.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A boolean value.
}
\description{
Check if a collection contains a value.
}
\section{Corner Cases}{
  Various types of \code{Na} are not-distinguished between.
  Type conversion is not carried out.
}
\examples{

NULL

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

