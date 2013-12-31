\name{xAsVariadic}
\alias{xAsVariadic}
\title{xAsVariadic}
\usage{
xAsVariadic(fn)
}
\arguments{
  \item{fn}{a unary function.}
}
\value{
a variadic function.
}
\description{
Return a variadic function that passes its arguments as a
list to ints underyling function.
}
\examples{

# xAsVariadic lets you take a non-unary function and
# creates a function that lets you pass it arguments
# as a single list.
#

# 1. create a version of sum that
}
\seealso{
Other function_application_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsUnary}};
\code{\link{xThread}}, \code{\link{xThread...}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xCompose}}, \code{\link{xCompose...}};
\code{\link{xConst}}, \code{\link{xK}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}};
\code{\link{xWait}}

Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xFlip}}; \code{\link{xFormals}};
\code{\link{xIsVariadic}}; \code{\link{xParams}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

