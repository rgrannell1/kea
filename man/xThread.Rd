\name{xThread}
\alias{xThread}
\title{xThread}
\usage{
xThread(init, fns)
}
\arguments{
  \item{init}{an arbitrary value}

  \item{fns}{several unary functions.}
}
\value{
a list.
}
\description{
Iteratively apply a value to list of functions.
}
\section{Corner Cases}{
  q returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xNot}};
\code{\link{xPartial}}; \code{\link{xVectorise}};
\code{\link{xWait}}
}

