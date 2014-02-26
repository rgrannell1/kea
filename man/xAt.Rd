\name{xAt}
\alias{xAt}
\alias{xAt...}
\title{xAt}
\usage{
xAt(num, coll)

xAt...(num, ...)
}
\arguments{
  \item{num}{a whole number. The index to select the
  collection at.  in \bold{coll}.}

  \item{coll}{a collection. The collection to subset.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Select a particular value from a collection.
}
\details{
\bold{xAt} is similar to base R's subsetting operator '[[',
except that it performs more validation on the input
indices, and it acts as a normal function.
}
\section{Corner Cases}{
  \bold{xAt} does not allow subscripting of values out of
  bounds.  If an index larger than the maximum value in
  \bold{coll} is given an error is thrown.
}
\examples{

NULL
}

