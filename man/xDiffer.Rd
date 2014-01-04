\name{xDiffer}
\alias{xDiffer}
\alias{xDiffer...}
\title{xDiffer}
\usage{
xDiffer(colls)

xDiffer...(...)
}
\arguments{
  \item{colls}{a collection of collections.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Get the asymettric set difference of several collections.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xIsMember}},
\code{\link{xIsMember...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}
}

