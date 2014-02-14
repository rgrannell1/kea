\name{xNotMember}
\alias{xNotMember}
\alias{xNotMember...}
\title{xNotMember}
\usage{
xNotMember(val, coll)

xNotMember...(val, ...)
}
\arguments{
  \item{val}{an arbitrary value. The value to test for
  non-membership in a collection.}

  \item{coll}{a collection. The collection to test elements
  from.}

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
\code{\link{xIsMember...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}; \code{\link{xUnionOf}},
\code{\link{xUnionOf...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

