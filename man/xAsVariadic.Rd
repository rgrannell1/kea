\name{xAsVariadic}
\alias{xAsVariadic}
\title{xAsVariadic}
\usage{
xAsVariadic(fn)
}
\arguments{
  \item{fn}{a unary function. The function to convert to a
  variadic function.}
}
\value{
A variadic function.
}
\description{
Return a variadic function that passes its arguments as a
list to ints underyling function.
}
\details{
\code{xAsVariadic} takes a unary function and returns a
function with ellipsis parametres.  The returned function
passes its ellipsis arguments as a single list to the
underlying function.
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

Other function_modifying_functions: \code{\link{\%and\%}},
\code{\link{\%or\%}}, \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{\%of\%}},
\code{\link{\%then\%}}, \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xCapture}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}

Other parametre_functions: \code{\link{xArityOf}};
\code{\link{xAsUnary}}; \code{\link{xFormalsOf}};
\code{\link{xIsVariadic}}; \code{\link{xParamsOf}};
\code{\link{xPartial}}, \code{\link{xPartial...}}
}

