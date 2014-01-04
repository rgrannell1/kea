\name{xIsMember}
\alias{xIsMember}
\alias{xIsMember...}
\title{xIsMember}
\usage{
xIsMember(val, coll)

xIsMember...(val, ...)
}
\arguments{
  \item{val}{an arbitrary value.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Check if a collection contains a value.
}
\section{Corner Cases}{
  various types of \code{Na} are not-distinguished between.
  Type conversion is not carried out.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other set_functions: \code{\link{xCombos}},
\code{\link{xCombos...}}; \code{\link{xDiffer}},
\code{\link{xDiffer...}}; \code{\link{xInter}},
\code{\link{xInter...}}; \code{\link{xSetProd}},
\code{\link{xSetProd...}}; \code{\link{xUnion}},
\code{\link{xUnion...}}
}

