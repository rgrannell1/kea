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

  \item{coll}{a collection. The collection to subset.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Select a collection at certain indices.
}
\details{
\bold{xAt} is similar to base R's subsetting operator '[',
except that it performs more validation on the input
indices, and it acts as a normal function.
}
\section{Corner Cases}{
  \bold{xAt} does not allow subscripting values out of
  bounds, unlike base R. If an index larger than the
  maximum value in \bold{coll} is given an error is thrown.
}
\examples{

NULL
}

