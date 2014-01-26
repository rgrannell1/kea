\name{xFlip}
\alias{xFlip}
\title{xFlip}
\usage{
xFlip(fn)
}
\arguments{
  \item{fn}{an arbitrary function. The function to have its
  formals reversed.}
}
\value{
A function of the same arity as \bold{fn}.
}
\description{
Reverse the formal parametres of a function.
}
\details{
\bold{xFlip} reverses the formal parametres of its input
function, including the defaults of those arguments.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}

Other parametre_functions: \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xFormalsOf}};
\code{\link{xIsVariadic}}; \code{\link{xParamsOf}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

