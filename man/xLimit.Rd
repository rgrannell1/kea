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
\section{Uses}{
  \code{xLimit} returns a function that behaves identically
  to the original input function, but has a limited
  lifespan. A natural application of \code{xLimit} is for
  creating functions that interact with web API's; most web
  API's only allow users to make a preset number of
  requests per hour. \code{xLimit} could be used to create
  functions with a built-in rate limit, to avoid having to
  writing rate-limit checking boilerplate.
}
\seealso{
Other time_functions: \code{\link{xDelay}};
\code{\link{xStopwatch}}
}

