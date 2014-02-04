\name{xIrrelevance}
\alias{xIrrelevance}
\title{xIrrelevance}
\usage{
xIrrelevance(...)
}
\arguments{
  \item{...}{arguments to be dropped.}
}
\value{
na
}
\description{
A function that always returns na.
}
\examples{

# 1. Using irrelevance as an identity
#    with xReject.

xReject(xIrrelevance, 1:10)
}
\seealso{
Other logical_constant_functions: \code{\link{xFalsity}};
\code{\link{xTruth}}
}

