\name{xElemNotNa}
\alias{xElemNotNa}
\alias{xElemNotNa...}
\title{xElemNotNa}
\usage{
xElemNotNa(coll)

xElemNotNa...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being non-na.}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Test every element in a collection for being \code{Na} of
any type.
}
\section{Corner Cases}{
  Returns logical(0) if \bold{coll} is length-zero.
}
\examples{

NULL

}

