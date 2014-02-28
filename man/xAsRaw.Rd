\name{xAsRaw}
\alias{xAsRaw}
\alias{xAsRaw...}
\title{xAsRaw}
\usage{
xAsRaw(raws)

xAsRaw...(...)
}
\arguments{
  \item{raws}{a collection of raw values. A list, pairlist
  or vector of length-one raw vectors to convert to a raw
  vector.}

  \item{...}{see above.}
}
\value{
A raw vector.
}
\description{
Convert a collection to a raw vector.
}
\details{
\code{xAsRaw} converts a list, pairlist or vector of
length-one raw numbers to a raw vector. It does not attempt
to convert non-raw collections to raw vectors.
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
\code{\link{xAsNamed}}
}

