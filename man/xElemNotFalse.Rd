\name{xElemNotFalse}
\alias{xElemNotFalse}
\alias{xElemNotFalse...}
\title{xElemNotFalse}
\usage{
xElemNotFalse(coll)

xElemNotFalse...(...)
}
\arguments{
  \item{coll}{a collection. The collection to test each
  element of for being non-false.}

  \item{...}{see above.}
}
\value{
A vector of boolean values.
}
\description{
Is an element of a collection not false?
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL

}

