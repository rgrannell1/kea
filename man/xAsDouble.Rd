\name{xAsDouble}
\alias{xAsDouble}
\alias{xAsDouble...}
\title{xAsDouble}
\usage{
xAsDouble(nums)

xAsDouble...(...)
}
\arguments{
  \item{nums}{a collection of double values. A list,
  pairlist or vector of length-one double vectors to
  convert to a double vector.}

  \item{...}{see above.}
}
\value{
A double vector.
}
\description{
Convert a collection to a double vector.
}
\details{
\code{xAsDouble} converts a list, pairlist or vector of
length-one double precision numbers to a double vector. It
does not attempt to convert non-double collections to
double vectors.
}
\section{Corner Cases}{
  Integer vectors are coerced to double vectors freely,
  since the difference is predominantly internal.
}
\examples{

NULL

}

