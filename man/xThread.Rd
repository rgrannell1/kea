\name{xThread}
\alias{xThread}
\alias{xThread...}
\title{xThread}
\usage{
xThread(val, fns)

xThread...(val, ...)
}
\arguments{
  \item{val}{an arbitrary value}

  \item{fns}{several unary functions.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Iteratively apply a value to list of functions.
}
\section{Corner Cases}{
  Returns the empty list if \code{coll} is length-zero.
}
\seealso{
Other function_application_functions: \code{\link{xApply}},
\code{\link{xApply...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

