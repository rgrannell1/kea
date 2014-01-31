\name{xAt}
\alias{xAt}
\alias{xAt...}
\title{xAt}
\usage{
xAt(nums, coll)

xAt...(nums, ...)
}
\arguments{
  \item{nums}{a vector of whole numbers. Indices to select
  values in \bold{coll}.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Select a collection at certain indices.
}
\details{
details
}
\section{Corner Cases}{
  \bold{xAt} does not allow subscripting values out of
  bounds, unlike base R. If an index larger than the
  maximum value in \bold{coll} is given an error is thrown.
}
\examples{

NULL
}

