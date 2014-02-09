\name{xOrder}
\alias{xOrder}
\alias{xOrder...}
\title{xOrder}
\usage{
xOrder(nums)

xOrder...(...)
}
\arguments{
  \item{nums}{a vector of numbers. The numbers to rank in
  order of size.}

  \item{...}{see above.}
}
\value{
A vector of numbers.
}
\description{
Return a permutation of indices that reorders an integer
vector.
}
\details{
\bold{xOrder} returns the indices for a collection that are
required to re-order it. For example,

\code{c(3, 1, 2)[ xOrder(c(3,1,2)) ]}

re-arranges the collection as

\code{c(1, 2, 3)}

This is a trivial use of \bold{xOrder}; a more common use
is to re-order a collection of collections by the indices
of one row or column.
}
\section{Corner Cases}{
  If nums is empty then \bold{integer(0)} is returned.
}
\examples{

NULL
}

