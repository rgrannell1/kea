\name{xWait}
\alias{xWait}
\title{xWait}
\usage{
xWait(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{num}{a nonnegative whole number.}
}
\value{
a function with the same parametres as \code{fn}.
}
\description{
Delay the evalution of a function after invokation for a
set amount of time.
}
\section{Corner Cases}{
  if \code{num} is zero then \code{fn} is returned
  untouched.
}
\seealso{
Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xFlip}};
\code{\link{xJuxtapose}}, \code{\link{xJuxtapose...}};
\code{\link{xLift}}, \code{\link{xLift...}};
\code{\link{xNot}}; \code{\link{xPartial}},
\code{\link{xPartial...}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}

Other time_functions: \code{\link{xLimit}};
\code{\link{xStopwatch}}
}

