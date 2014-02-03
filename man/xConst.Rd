\name{xConst}
\alias{xConst}
\alias{xK}
\title{xConst}
\usage{
xConst(val)

xK(val)
}
\arguments{
  \item{val}{an arbitrary value. The value to close over
  with a closure.}
}
\value{
A variadic function that returns \bold{val}.
}
\description{
Create a function that returns a particular value.
}
\details{
\bold{xConst} is the contant combinator - a function that
takes an arguments and returns a function that always
returns that value.

The function returned by \bold{xConst} has ellipsis
arguments, and ignores those arguments.
}
\examples{

NULL

}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

