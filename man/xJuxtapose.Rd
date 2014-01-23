\name{xJuxtapose}
\alias{xJuxtapose}
\alias{xJuxtapose...}
\title{xJuxtapose}
\usage{
xJuxtapose(fns)

xJuxtapose...(...)
}
\arguments{
  \item{fns}{a list or pairlist of functions.}

  \item{...}{see above.}
}
\value{
A variadic function.
}
\description{
Create a function that applies an argument to a list of
underlying functions.
}
\details{
\code{xJuxtapose} is a method of applying one value to
several functions simultaneously. The function returned by
\code{xJuxtapose} returns a list - on element for each
input function to \code{xJuxtapose}.  Each element of this
list contains the result of calling an element of
\code{fns} with the arguments supplied to
\code{xJuxtapose}'s return function.
}
\section{Corner Cases}{
  If no functions are provided the empty list is returned.
}
\seealso{
Other function_combining_functions: \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xLift}},
\code{\link{xLift...}}

Other function_modifying_functions:
\code{\link{xAsClosure}}; \code{\link{xAsFunction}},
\code{\link{xAsFunction...}}; \code{\link{xAsUnary}};
\code{\link{xAsVariadic}}; \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xConst}},
\code{\link{xK}}; \code{\link{xDelay}};
\code{\link{xFlip}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}
}

