\name{xDelay}
\alias{xDelay}
\title{xDelay}
\usage{
xDelay(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function. The function to slow the
  rate of execution of.}

  \item{num}{a nonnegative whole number. The number of
  seconds to delay execution by.}
}
\value{
A function with the same parametres as \bold{fn}.
}
\description{
Delay the evalution of a function after invokation for a
set amount of time.
}
\details{
\bold{xDelay} is primarily meant for use with
side-effectful functions; likely examples including
functions that call web API's and functions that that
display graphics.

\bold{xDelay} throttles the rate at which a function can be
repeatedly excecuted. This can be useful in preventing API
rate limits from being exceeded or to emulate a refresh
rate.
}
\section{Corner Cases}{
  If \bold{num} is zero then \bold{fn} is returned
  untouched.
}
\examples{

NULL

}
\seealso{
Other function_modifying_functions: \code{\link{\%and\%}},
\code{\link{\%or\%}}, \code{\link{xLift}},
\code{\link{xLift...}}; \code{\link{\%of\%}},
\code{\link{\%then\%}}, \code{\link{xCompose}},
\code{\link{xCompose...}}; \code{\link{xAsClosure}};
\code{\link{xAsUnary}}; \code{\link{xAsVariadic}};
\code{\link{xCapture}}, \code{\link{xK}};
\code{\link{xFlip}}; \code{\link{xJuxtapose}},
\code{\link{xJuxtapose...}}; \code{\link{xNot}};
\code{\link{xPartial}}, \code{\link{xPartial...}};
\code{\link{xThread}}, \code{\link{xThread...}};
\code{\link{xVectorise}}, \code{\link{xVectorize}}

Other inpure_functions: \code{\link{xDo}},
\code{\link{xDo...}}; \code{\link{xExecute}};
\code{\link{xStopwatch}}; \code{\link{xVersion}}

Other time_functions: \code{\link{xLimit}};
\code{\link{xStopwatch}}
}

