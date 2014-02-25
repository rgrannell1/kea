\name{xLift}
\alias{\%and\%}
\alias{\%or\%}
\alias{xLift}
\alias{xLift...}
\title{xLift}
\usage{
xLift(fn, fns)

xLift...(fn, ...)

fn1 \%or\% fn2

fn1 \%and\% fn2
}
\arguments{
  \item{fn}{a binary function.}

  \item{fns}{a collection functions.}

  \item{...}{see above.}
}
\value{
Returns a unary function of x.
}
\description{
Compose a function with two other functions.
}
\details{
\bold{xLift} takes a function that works on some type of
value, and makes that function work on \ital{functions of
those values}.

Two partially applied version of \bold{xLift} exist;
\%and\% & \%or\%.  These are primarily intended for use
with higher-order functions that take a predicate, allowing
several predicates to be stringed together into a new
predicate.

\code{xSelect(is.integer %or% is.complex, list(1L, 1+1i,
2L, 'string'))}

\code{list(1L, 1+1i, 2L, 'string')}
}
\examples{

NULL

}
\seealso{
Other function_combining_functions: \code{\link{\%of\%}},
\code{\link{\%then\%}}, \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}

Other function_modifying_functions: \code{\link{\%of\%}},
\code{\link{\%then\%}}, \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCapture}}, \code{\link{xK}};
\code{\link{xDelay}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

