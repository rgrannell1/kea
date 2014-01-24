\name{xPack}
\alias{xPack}
\alias{xPack...}
\title{xPack}
\usage{
xPack(coll)

xPack...(...)
}
\arguments{
  \item{coll}{a collection. The collection to remove empty
  values from.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Remove all length-zero values from a collection.
}
\details{
\bold{xPack} selects the non-empty elements from a
collection. Zero-length vectors like \bold{integer(0)} and
\bold{Null} are removed from the input collection.
}
\section{Corner Cases}{
  Returns the emty list if \bold{coll} is length-zero, or
  all elements in \bold{coll} are length-zero.
}
\seealso{
Other filtering_functions: \code{\link{xPartition}};
\code{\link{xReject}}, \code{\link{xReject...}};
\code{\link{xRemoveEmpty}}, \code{\link{xRemoveEmpty...}};
\code{\link{xRemoveNan}}; \code{\link{xRemoveNa}};
\code{\link{xRemoveNull}}, \code{\link{xRemoveNull...}};
\code{\link{xSelect}}, \code{\link{xSelect...}}
}

