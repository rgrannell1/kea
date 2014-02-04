\name{xFalsity}
\alias{xFalsity}
\title{xFalsity}
\usage{
xFalsity(...)
}
\arguments{
  \item{...}{arguments to be dropped.}
}
\value{
false
}
\description{
A function that always returns false.
}
\examples{

# 1. Using falsity as an identity
#    with xReject.

xReject(xFalsity, 1:10)

}
\seealso{
Other logical_constant_functions:
\code{\link{xIrrelevance}}; \code{\link{xTruth}}
}

