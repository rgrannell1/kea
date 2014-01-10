\name{xIterate}
\alias{xIterate}
\title{xIterate}
\usage{
xIterate(fn, val)
}
\arguments{
  \item{fn}{a function.}

  \item{val}{an arbitrary value.}
}
\value{
The result of successively applying \code{f} to \code{val}.
}
\description{
Iteratively apply a function to a value.
}
\section{Corner Cases}{
  Length-zero values of \code{val} are treated as normal
  values, since \code{val} accepts an arbitrary value. Like
  while loops, \code{xIterate} is potentially
  non-terminating, and \code{Return( )} must be called to
  terminate the function.
}
\examples{

# Example 1.
#
# find the smallest number evenly divisible by 1, 2, ..., 10
# this works a lot like a while loop that returns a value.

xIterate(
	num := {

		evenly_divisible <- num \%\% (1:10) == 0

		if (all(evenly_divisible)) {
			Return(num)
		} else {
			num + 1
		}
	},
	1
)

2520
}

