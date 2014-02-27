\name{xThread}
\alias{xThread}
\alias{xThread...}
\title{xThread}
\usage{
xThread(val, fns)

xThread...(val, ...)
}
\arguments{
  \item{val}{an arbitrary value. The value to feed to the
  input functions.}

  \item{fns}{a collection of unary functions. The functions
  to successively pipe the value through.}

  \item{...}{see above.}
}
\value{
An arbitrary value.
}
\description{
Iteratively apply a list of functions to a value.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{

NULL

}
\seealso{
Other function_application_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}

Other function_modifying_functions: \code{\link{\%and\%}},
\code{\link{\%or\%}}, \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{\%of\%}},
\code{\link{\%then\%}}, \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCapture}}, \code{\link{xK}};
\code{\link{xDelay}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

