\name{xMap}
\alias{xMap}
\alias{xMap...}
\title{xMap}
\usage{
xMap(fn, coll)

xMap...(fn, ...)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Apply a function to each element of a collection.
}
\section{Uses}{
  xMap is used to apply the same modification to every
  element in a collection. For example, xMap can be used to
  convert every integer in a list to a string, to lookup
  every URL in a list of URLs, or to multiply every integer
  in a list by two.
}

\section{Corner Cases}{
  Returns the empty list is \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

