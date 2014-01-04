\name{xMapply}
\alias{xMapply}
\alias{xMapply...}
\title{xMapply}
\usage{
xMapply(fn, coll)

xMapply...(fn, ...)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Apply a function to each element of a collection.
}
\section{Uses}{
  Map is used to apply the same modification to every
  element in a collection. For example, map can be used to
  convert every integer in a list to a string, to lookup
  every URL in a list of URLs, or to multiply every integer
  in a list by two.
}

\section{Corner Cases}{
  returns the empty list is \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other mapping_functions: \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

