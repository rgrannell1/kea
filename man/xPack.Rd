\name{xPack}
\alias{xPack}
\alias{xPack...}
\title{xPack}
\usage{
xPack(coll)

xPack...(...)
}
\arguments{
  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
Returns a list of elements in \code{coll}, with all
length-zero values removed.
}
\description{
Remove all length-zero values from a collection.
}
\section{Corner Cases}{
  Returns the emty list if \code{coll} is length-zero, or
  all elements in \code{coll} are length-zero.
}
\seealso{
Other filtering_functions: \code{\link{xPartition}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xRemoveEmpty}}, \code{\link{xRemoveEmpty...}};
\code{\link{xRemoveNan}}; \code{\link{xRemoveNa}};
\code{\link{xRemoveNull}}, \code{\link{xRemoveNull...}};
\code{\link{xSelect}}, \code{\link{xSelect...}}
}

