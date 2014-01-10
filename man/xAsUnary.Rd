\name{xAsUnary}
\alias{xAsUnary}
\title{xAsUnary}
\usage{
xAsUnary(fn)
}
\arguments{
  \item{fn}{an arbitrary function.}
}
\value{
a unary function of x.
}
\description{
Convert any function to a unary function.
}
\details{
xAsUnary takes a function and returns a function that has
one argument. The first element of the argument is passed
to the first parametre of the underlying function, the
second element to the second parametre, and so on.
}
\examples{

# 1. xAsUnary can be used to adapt higher-order
#    functions to make them work with non-unary functions.

xSelect...(
    xAsUnary(
        (a : b) := {
            a > 0 && b >= 0
        }
    ),
    list(+1, +1),
    list(+0, +1),
    list(-1, 1),
    list(-1, -1)
)

list(+1, +1)
}
\seealso{
Other function_application_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsVariadic}};
\code{\link{xThread}}, \code{\link{xThread...}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsVariadic}};
\code{\link{xCompose}}, \code{\link{xCompose...}};
\code{\link{xConst}}, \code{\link{xK}};
\code{\link{xDelay}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}

Other parametre_functions: \code{\link{xAsVariadic}};
\code{\link{xFlip}}; \code{\link{xFormalsOf}};
\code{\link{xIsVariadic}}; \code{\link{xParamsOf}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

