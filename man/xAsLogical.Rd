\name{xAsLogical}
\alias{xAsLogical}
\alias{xAsLogical...}
\title{xAsLogical}
\usage{
xAsLogical(bools)

xAsLogical...(...)
}
\arguments{
  \item{bools}{a collection of logical values.}

  \item{...}{see above.}
}
\value{
A logical vector.
}
\description{
Convert a collection to a logical vector.
}
\details{
\code{xAsLogical} converts a list, pairlist or vector of
length-one logical values to a logical vector. It does not
attempt to convert non-logical collections to logical
vectors.
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
\code{\link{xAsRaw}}, \code{\link{xAsRaw...}}
}

