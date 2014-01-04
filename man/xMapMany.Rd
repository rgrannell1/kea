\name{xMapMany}
\alias{xMapMany}
\alias{xMapMany...}
\title{xMapMany}
\usage{
xMapMany(fn, colls)

xMapMany...(fn, ...)
}
\arguments{
  \item{fn}{a n-ary function.}

  \item{colls}{n collections.}

  \item{...}{see above.}
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

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other mapping_functions: \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xRecurMap}},
\code{\link{xRecurMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

