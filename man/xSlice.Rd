\name{xSlice}
\alias{xSlice}
\alias{xSlice...}
\title{xSlice}
\usage{
xSlice(nums, coll)

xSlice...(nums, ...)
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
\bold{xSlice} is similar to base R's subsetting operator
'[', except that it performs more validation on the input
indices, and it acts as a normal function.
}
\section{Corner Cases}{
  \bold{xSlice} does not allow subscripting values out of
  bounds, unlike base R. If an index larger than the
  maximum value in \bold{coll} is given an error is thrown.
}
\examples{

NULL
}

