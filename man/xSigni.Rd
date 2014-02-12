\name{xSigni}
\alias{xSigni}
\alias{xSigni...}
\title{xSigni}
\usage{
xSigni(nums)

xSigni...(...)
}
\arguments{
  \item{nums}{a collection of numbers. The numbers to take
  the sign of.}

  \item{...}{see above.}
}
\value{
A integer vector of elements in the set \code{{-1, 0, +1}}.
}
\description{
Get the sign of a vector of numbers.
}
\section{Corner Cases}{
  If \code{nums} is lenth-zero then the unit of that vector
  is returned.  The sign of zero is zero.
}
\examples{

NULL

}

