\name{xMapWhen}
\alias{xMapWhen}
\alias{xMapWhen...}
\title{xMapWhen}
\usage{
xMapWhen(pred, fn, coll)

xMapWhen...(pred, fn, ...)
}
\arguments{
  \item{pred}{a predicate function.}

  \item{fn}{a unary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
a list.
}
\description{
Selectively apply a function to elements in a collection.
}
\section{Corner Cases}{
  returns the empty list if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other mapping_functions: \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

