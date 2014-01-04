\name{xUnion}
\alias{xUnion}
\alias{xUnion...}
\title{xUnion}
\usage{
xUnion(colls)

xUnion...(...)
}
\arguments{
  \item{colls}{a collection of collections.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Get the set union of several collections.
}
\section{Corner Cases}{
  returns the empty list if \code{colls} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}
}

