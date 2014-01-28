\name{xNotMember}
\alias{xNotMember}
\alias{xNotMember...}
\title{xNotMember}
\usage{
xNotMember(val, coll)

xNotMember...(val, ...)
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
Check if a collection doesn't contain a value.
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
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

