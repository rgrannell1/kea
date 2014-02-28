\name{xAsNamed}
\alias{xAsNamed}
\title{xAsNamed}
\usage{
xAsNamed(strs, coll)
}
\arguments{
  \item{strs}{a collection of strings. The names to add to
  the input collection.}

  \item{coll}{a collection. The collection to add names
  to.}
}
\value{
A named list.
}
\description{
Add names to a collection.
}
\details{
\bold{xAsNamed} is similar to the in-place assignment
function \bold{names<-}, except that it is not an in-place
assignment function.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
  Both duplicated and length-zero names are allowed.
}
\examples{

NULL

}
\seealso{
Other container_conversion_functions:
\code{\link{xAsCharacter}}, \code{\link{xAsCharacter...}};
\code{\link{xAsComplex}}, \code{\link{xAsComplex...}};
\code{\link{xAsDouble}}, \code{\link{xAsDouble...}};
\code{\link{xAsInteger}}, \code{\link{xAsInteger...}};
\code{\link{xAsLogical}}, \code{\link{xAsLogical...}};
\code{\link{xAsRaw}}, \code{\link{xAsRaw...}}

Other name_functions: \code{\link{xGetKey}};
\code{\link{xPluck}}, \code{\link{xPluck...}};
\code{\link{xUnzipNames}}, \code{\link{xUnzipNames...}};
\code{\link{xZipNames}}, \code{\link{xZipNames...}}
}

