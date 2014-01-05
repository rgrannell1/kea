\name{xDeepMap}
\alias{xDeepMap}
\alias{xDeepMap...}
\title{xDeepMap}
\usage{
xDeepMap(fn, coll)

xDeepMap...(fn, ...)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a list or pairlist.}

  \item{...}{see above.}
}
\value{
a list or pairlist.
}
\description{
Recursively map a function into a nested collection,
preserving its structure.
}
\section{Variadic Functions

\link{ help_variadic }}{

}
\seealso{
Other mapping_functions: \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapWhen}},
\code{\link{xMapWhen...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

