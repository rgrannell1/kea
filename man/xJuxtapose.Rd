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
a variadic function.
}
\description{
Create a function that applies an argument to a list of
underlying functions.
}
\section{Corner Cases}{
  If no functions are provided the empty list is returned.
}

\section{Variadic Functions

\link{ help_variadic }}{

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
\code{\link{xK}}; \code{\link{xFlip}}; \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xSlow}}; \code{\link{xThread}},
\code{\link{xThread...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

