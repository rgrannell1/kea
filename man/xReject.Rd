\name{xReject}
\alias{xReject}
\alias{xReject...}
\title{xReject}
\usage{
xReject(pred, coll)

xReject...(pred, ...)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Remove all elements from a collection matching a predicate.
}
\details{
\code{xReject} applies a predicate function to each element
of the input collection, and returns the elements of that
collection such that the predicate returned false or na.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero, or
  no match is found. If the predicate returns a non-logical
  value an error is thrown. If an na value is returned by
  the predicate it is treated as a false value.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other filtering_functions: \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xPartition}};
\code{\link{xRemoveEmpty}}, \code{\link{xRemoveEmpty...}};
\code{\link{xRemoveNan}}; \code{\link{xRemoveNa}};
\code{\link{xRemoveNull}}, \code{\link{xRemoveNull...}};
\code{\link{xSelect}}, \code{\link{xSelect...}}
}

