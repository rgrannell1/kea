\name{xMapMany}
\alias{xMapMany}
\title{xMapMany}
\usage{
  xMapMany(fn, colls)
}
\arguments{
  \item{fn}{a n-ary function.}

  \item{colls}{n collections.}
}
\value{
  a list.
}
\description{
  Map a function across many collections simultaneously.
}
\section{Corner Cases}{
  returns the empty list if any collection is length-zero.
  If the collection lengths are not equal then elements are
  recycled in the shorter collections.
}
\seealso{
  Other higher_order_functions map_like_functions
  collection_functions: \code{\link{xFlatMap}},
  \code{\link{xMap}}, \code{\link{xMapIndexed}},
  \code{\link{xMapWhen}}, \code{\link{xRecurMap}}
}

