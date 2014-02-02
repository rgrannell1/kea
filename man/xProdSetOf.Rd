\name{xProdSetOf}
\alias{xProdSetOf}
\alias{xProdSetOf...}
\title{xProdSetOf}
\usage{
xProdSetOf(colls)

xProdSetOf...(...)
}
\arguments{
  \item{colls}{a collection of collections. The collections
  to use to take the cartesian product.}

  \item{...}{see above.}
}
\value{
A list of collections, with as many elements per collection
as there are collections in \bold{colls}.
}
\description{
Get the cartesian product of several collections.
}
\section{Corner Cases}{
  Returns the empty list if \bold{colls} is length-zero.
}
\examples{

# 1. xProdSetOf with xMap, xMapply or xReduce are
#    generally very nice replacements for loops.

xMapply(
    (ith : jth) := {
        ith^2 + jth^2
    },
    xProdSetOf...(1:4, 1:4)
)

# is roughly the same as base R's...

res <- list()

for (ith in 1:4) {
    for (jth in 1:4) {
        res <- c(res, ith^2 + jth^2)
    }
}

res

# 2. Compute the standard childhood multiplication
#    table.

xProdSetOf...(1:12, 1:12)
}
\seealso{
Other combinatoric_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}

Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

