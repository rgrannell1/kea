\name{xStopwatch}
\alias{xStopwatch}
\title{xStopwatch}
\usage{
xStopwatch(num)
}
\arguments{
  \item{num}{a positive number.}
}
\value{
a variadic function that ignores its arguments.
}
\description{
Create a function that returns true for a preset time after
creation.
}
\section{Uses}{
  \code{xStopwatch} is used to measure the passage of time
  beyond a threshold. \code{xStopwatch} is used internally
  by Arrow's testing suite to check that tests are run for
  a certain amount of time, but no longer. By default, unit
  tests are run until a 0.1 second \code{xStopwatch}
  returns false. The tests then terminate.
}
\seealso{
Other time_functions: \code{\link{xLimit}};
\code{\link{xSlow}}
}

