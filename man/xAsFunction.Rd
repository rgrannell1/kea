\name{xAsFunction}
\alias{xAsFunction}
\alias{xAsFunction...}
\title{xAsFunction}
\usage{
xAsFunction(coll)

xAsFunction...(...)
}
\arguments{
  \item{coll}{a collection}

  \item{...}{see above.}
}
\value{
A function that takes one or more indices.
}
\description{
Convert a collection to a function that takes an index.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll is length-zero}.
}

\section{Variadic Functions

\link{ help_variadic }}{

}
\examples{

}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

