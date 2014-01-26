\name{xLift}
\alias{xLift}
\alias{xLift...}
\title{xLift}
\usage{
xLift(fn, fns)

xLift...(fn, ...)
}
\arguments{
  \item{fn}{a binary function.}

  \item{fns}{a list or pairlist of binary functions.}

  \item{...}{see above.}
}
\value{
Returns a unary function of x.
}
\description{
Compose a binary function with two other functions.
}
\details{
Two partially applied version of \bold{xLift} exist;
\bold{\%or\%} and \bold{\%and\%}. These exist to solve

\code{is.na(x) || is.null(x) || is.nan(x)}

\code{(is.na \%or\% is.null \%or\% is.nan)(x)}
}
\examples{

NULL

}
\seealso{
Other function_combining_functions: \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

