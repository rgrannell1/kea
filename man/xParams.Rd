\name{xParams}
\alias{xParams}
\title{xParams}
\usage{
xParams(fn)
}
\arguments{
  \item{fn}{an arbitrary function or primitive function.}
}
\value{
a character vector/.
}
\description{
Get the parametre names of a function or primitive
function.
}
\section{Corner Cases}{
  If \code{fn} is a primitive function a heuristic is used
  to obtain its parametre names, which may not work for all
  functions.
}
\seealso{
Other higher_order_functions: \code{\link{xApply}};
\code{\link{xArity}}; \code{\link{xAsUnary}};
\code{\link{xCompose}}; \code{\link{xConst}};
\code{\link{xFlip}}; \code{\link{xFormals}};
\code{\link{xIterate}}; \code{\link{xJuxtapose}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xNot}}; \code{\link{xStopWatch}};
\code{\link{xUnfold}}; \code{\link{xWait}}
}

