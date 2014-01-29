\name{xCompose}
\alias{xCompose}
\alias{xCompose...}
\title{xCompose}
\usage{
xCompose(fns)

xCompose...(...)
}
\arguments{
  \item{fns}{a collection of functions. The functions to
  compose with each other.}

  \item{...}{see above.}
}
\value{
A variadic function.
}
\description{
Take several functions and return a composite function.
}
\details{
Arrow's function composition is largely done with the infix
operators \%of\% and \%then\%.

\bold{\%then\%} is the classic function composition
operator;

\code{(as.numeric %then% sqrt %then% print)('10')}

Like methods, the order of function execution is from left
to right; convert a string to a number then take the square
root then print the value. The reversed form \%of\% is
equally as ubiquitous.

The classic example of function composition (apart from
Church numerals) is composing linear functions.

\code{f <- x := 2*x}

\code{g <- x := 4*x}

\code{h <- x := 1*x}

\code{(f \%of\% g \%of\% h)(1)}

\code{(x := 2*(4*(1*x)) )(1)}

8

In this case the output of one function is piped to
another:

\code{1 => 1*1 => 1*4 => 4*2 => 8}

In this case function composition behaves exactly like
multiplication.

In more typical use of \bold{xCompose} is to reduce the
number of anonymous functions needed by some programs.

\code{xMap(sqrt \%of\% sqrt, 1:10)}
}
\examples{

}
\seealso{
Other function_combining_functions:
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

