\name{xPartition}
\alias{xPartition}
\title{xPartition}
\usage{
xPartition(pred, coll)
}
\arguments{
  \item{pred}{a predicate.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
Returns two lists; a list of elements in a collection for
which a predicate returns true, and a list of elements in a
collection for which a predicate returns false or na.
}
\description{
Divide elements in a collection into two collections based
on a predicate function.
}
\section{Corner Cases}{
  if \code{coll} is empty the empty list is returned. If
  all the elements return only true/only false, then one of
  two sublists will be the empty list.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other filtering_functions: \code{\link{xFilter}},
\code{\link{xFilter...}}, \code{\link{xSelect}},
\code{\link{xSelect...}}; \code{\link{xPack}},
\code{\link{xPack...}}; \code{\link{xReject}},
\code{\link{xReject...}}
}

