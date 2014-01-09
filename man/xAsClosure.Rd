\name{xAsClosure}
\alias{xAsClosure}
\title{xAsClosure}
\usage{
xAsClosure(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
a function (closure).
}
\description{
Convert a primitive function to a closure.
}
\section{Corner Cases}{
  xAsClosure does not work for every primitive function
  (for example 'c'), so caution should be taken when using
  this function.
}
\examples{

# 1. The most likely use case; converting the
#    primitive operations to normal R functions.

plus <- xAsClosure('+')
plus(1, 2)

3

is.primitive(plus)

FALSE
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsFunction}}, \code{\link{xAsFunction...}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}, \code{\link{xCompose...}};
\code{\link{xConst}}, \code{\link{xK}};
\code{\link{xDelay}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

