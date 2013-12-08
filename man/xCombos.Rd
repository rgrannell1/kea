\name{xCombos}
\alias{xCombos}
\title{xCombos}
\usage{
  xCombos(num, coll)
}
\arguments{
  \item{num}{a nonnegative whole number.}

  \item{coll}{a collection}
}
\value{
  a list of lists, with each list containing \code{num}
  elements.
}
\description{
  Generate all ways of choosing several elements from a
  collection.
}
\section{Corner Cases}{
  returns the empty list if \code{num} is zero.
}
\seealso{
  Other combinatoric_functions collection_functions:
  \code{\link{xSetProd}}
}

