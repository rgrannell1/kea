\name{xCapture}
\alias{xCapture}
\alias{xK}
\title{xCapture}
\usage{
xCapture(val)

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
\bold{xCapture} is the contant combinator - a function that
takes an arguments and returns a function that always
returns that value.

The function returned by \bold{xCapture} has ellipsis
arguments, and ignores those arguments.
}
\examples{

NULL

}
\seealso{
Other basic_functions: \code{\link{xFalsity}};
\code{\link{xIrrelevance}}; \code{\link{xI}},
\code{\link{xIdentity}}; \code{\link{xTruth}}

Other function_modifying_functions: \code{\link{\%and\%}},
\code{\link{\%or\%}}, \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{\%of\%}},
\code{\link{\%then\%}}, \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xDelay}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

