\name{xInter}
\alias{xInter}
\alias{xInter...}
\title{xInter}
\usage{
xInter(colls)

xInter...(...)
}
\arguments{
  \item{colls}{a collection of collections.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Get the set intersection of several collections.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xDuplicated}},
\code{\link{xDuplicated...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}; \code{\link{xUnique}},
\code{\link{xUnique...}}
}

