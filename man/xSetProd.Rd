\name{xSetProd}
\alias{xSetProd}
\alias{xSetProd...}
\title{xSetProd}
\usage{
xSetProd(colls)

xSetProd...(...)
}
\arguments{
  \item{colls}{n collections.}

  \item{...}{see above.}
}
\value{
A list of n-element lists.
}
\description{
Get the cartesian product of several collections.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\examples{

# 1. xSetProd with xMap, xMapply or xReduce are
#    generally very nice replacements for loops.

xMapply(
    (ith : jth) := {
        ith^2 + jth^2
    },
    xSetProd...(1:4, 1:4)
)

# is roughly the same as base R's...

res <- list()

for (ith in 1:4) {
    for (jth in 1:4) {
        res <- c(res, ith^2 + jth^2)
    }
}

res
}
\seealso{
Other combinatoric_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}

Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xDuplicated}},
\code{\link{xDuplicated...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUnique}},
\code{\link{xUnique...}}
}

