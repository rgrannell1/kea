\name{xPowerSetOf}
\alias{xPowerSetOf}
\alias{xPowerSetOf...}
\title{xPowerSetOf}
\usage{
xPowerSetOf(coll)

xPowerSetOf...(...)
}
\arguments{
  \item{coll}{a collection. The collection to return the
  subsets of.}

  \item{...}{see above.}
}
\value{
A list of lists.
}
\description{
Enumerate every way of subsetting a collection.
}
\details{
\bold{xPowerSetOf} generates the set of all subsets of a
collection.  This set has length \bold{2^length(coll)}.
}
\examples{

# 1. generate all integer partitions of a number with distinct parts
#    The partition of a number n is a way or
#    writing n as the sum of positive integers.
#    In this case we are looking for partitions with distinct parts;
#    no number can be repeated.

# The partitions of 5 with distinct parts are
#
# 5
# 4 + 1
# 3 + 2

# These partitions are a subset of the product set
# of 1...5.

x_(1:5) $
xPowerSetOf() $
xSelect(xs := {
	(unlist \%then\% sum)(xs) == 5
})

# list(
#     list(2, 3),
#     list(1, 4),
#     list(5)
# )
}
\seealso{
Other combinatoric_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}

Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}; \code{\link{xUnionOf}},
\code{\link{xUnionOf...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

