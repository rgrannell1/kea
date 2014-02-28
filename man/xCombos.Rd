\name{xCombos}
\alias{xCombos}
\alias{xCombos...}
\title{xCombos}
\usage{
xCombos(num, coll)

xCombos...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number. The number of
  elements to choose from \bold{coll}.}

  \item{coll}{a collection. The collection to draw elements
  from.}

  \item{...}{see above.}
}
\value{
A list of lists, with each list containing \bold{num}
elements.
}
\description{
Enumerate all ways of choosing several elements from a
collection.
}
\details{
\bold{xCombos} enumerates all ways of choosing \bold{num}
distinct elements from a larger collection, where order
doesn't matter.

The number of ways of choosing \bold{num} elements from a
collection is given by the base R function
\code{choose(num, length(coll))}.
}
\section{Corner Cases}{
  Returns the empty list if \bold{num} is zero.
}
\examples{

NULL

}
\seealso{
Other combinatoric_functions: \code{\link{xCycle}},
\code{\link{xCycle...}}; \code{\link{xPermute}},
\code{\link{xPermute...}}; \code{\link{xPowerSetOf}},
\code{\link{xPowerSetOf...}}; \code{\link{xProdSetOf}},
\code{\link{xProdSetOf...}}
}

