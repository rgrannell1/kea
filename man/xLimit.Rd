\name{xLimit}
\alias{xLimit}
\title{xLimit}
\usage{
xLimit(fn, num)
}
\arguments{
  \item{fn}{an arbitrary function.}

  \item{num}{a positive whole number.}
}
\value{
A function with the same parametres as \code{fn}.
}
\description{
Create a function that can call its underlying function a
limited number of times.
}
\seealso{
Other time_functions: \code{\link{xDelay}};
\code{\link{xStopwatch}}
}

