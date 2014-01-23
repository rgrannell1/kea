\name{xCombos}
\alias{xCombos}
\alias{xCombos...}
\title{xCombos}
\usage{
xCombos(num, coll)

xCombos...(num, ...)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list of lists, with each list containing \code{num}
elements.
}
\description{
Generate all ways of choosing several elements from a
collection.
}
\section{Corner Cases}{
  Returns the empty list if \code{num} is zero.
}
\seealso{
Other combinatoric_functions: \code{\link{xSetProd}},
\code{\link{xSetProd...}}

Other set_functions: \code{\link{xDuplicated}},
\code{\link{xDuplicated...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xNotMember}},
\code{\link{xNotMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUnique}},
\code{\link{xUnique...}}
}

