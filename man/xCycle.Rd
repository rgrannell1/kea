\name{xCycle}
\alias{xCycle}
\alias{xCycle...}
\title{xCycle}
\usage{
xCycle(num, colls)

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
\details{
Cyclic permutations are an important combinatorial object.
There are four cyclic permutations of a length-four
collection:

\code{[1, 2, 3, 4]}

\code{[2, 3, 4, 1]}

\code{[3, 4, 1, 2]}

\code{[4, 1, 2, 3]}

Which map to

\code{xCycle...(0, 1:4)}

\code{xCycle...(1, 1:4)}

\code{xCycle...(2, 1:4)}

\code{xCycle...(3, 1:4)}
}
\section{Corner Cases}{
  If \bold{coll} is empty the empty list is returned.
}
\examples{

# 1. Generate all cyclic permutations of a list
#    This is really easy to do with xCycle.

all_cyclic_permutations <- coll := {
    xMap(
    	xPartial...(xCycle, coll = coll),
        seq_along(coll) - 1
    )
}

all_cyclic_permutations(letters[1:3])

# list(
#     list('a', 'b', 'c'),
#     list('b', 'c', 'a'),
#     list('c', 'a', 'b')
# )
}
\seealso{
Other combinatoric_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}

Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDuplicatesOf}},
\code{\link{xDuplicatesOf...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}; \code{\link{xUnionOf}},
\code{\link{xUnionOf...}}; \code{\link{xUniqueOf}},
\code{\link{xUniqueOf...}}
}

