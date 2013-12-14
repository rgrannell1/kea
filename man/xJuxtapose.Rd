\name{xJuxtapose}
\alias{xJuxtapose}
\title{xJuxtapose}
\usage{
xJuxtapose(fns)
}
\arguments{
  \item{fns}{a list or pairlist of functions.}
}
\value{
a variadic function.
}
\description{
Create a function that applies an argument to a list of
underlying functions.
}
\section{Corner Cases}{
  If no functions are provided the empty list is returned.
}
\seealso{
Other higher_order_functions: \code{\link{xArity}};
\code{\link{xAsUnary}}; \code{\link{xCompose}};
\code{\link{xConst}}; \code{\link{xFlip}};
\code{\link{xFormals}}; \code{\link{xIterate}};
\code{\link{xLift}}; \code{\link{xLimit}};
\code{\link{xNot}}; \code{\link{xParams}};
\code{\link{xStopWatch}}; \code{\link{xUnfold}};
\code{\link{xWait}}
}

