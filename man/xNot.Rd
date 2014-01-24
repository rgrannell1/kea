\name{xNot}
\alias{xNot}
\title{xNot}
\usage{
xNot(pred)
}
\arguments{
  \item{pred}{a predicate. The predicate function to return
  a negation of.}
}
\value{
A predicate function with the same parametres as
\code{pred}.
}
\description{
Return the logical negation of a function.
}
\details{
\bold{xNot} takes a predicate function - such as
\bold{is.integer( )} - and returns the negation of that
function - \bold{not.integer( )}.

\code{not.integer <- xNot(is.integer)}

\code{is.integer(1L)}

\code{True}

\code{not.integer(1L)}

\code{False}

In general, if a function \bold{fn} returns true
\bold{xNot(fn)} returns false for the same value.
Similarily, if \bold{fn} returns false then \bold{xNot(fn)}
returns true for that value. Finally (R uses three-value
logic) if \bold{fn} returns na for a value so does its
negation.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

