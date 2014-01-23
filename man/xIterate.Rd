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
The result of successively applying \bold{fn} to
\bold{val}.
}
\description{
Iteratively apply a function to a value.
}
\details{
\bold{xIterate} repeatedly calls a function on an initial
value until it is explicitely halted using \bold{Return(
)}. It is similar for a while loop, with the added benefits
of an explicit return value and easier debugging.

The only way to end \bold{xIterate} is to call
\bold{Return( )}. A trivial example of ending an
\bold{xIterate} call is given below.

\code{xIterate(num := if (num >= 10) Return(num) else num +
1, 0)}

\code{10}

The above call iterates from zero to ten, before returning
the last number it encounters - ten. More useful examples
are given below, but the above example demonstrates the
basic usage of \bold{xIterate}.
}
\section{Corner Cases}{
  Length-zero values of \bold{val} are treated as normal
  values, since \bold{val} accepts an arbitrary value. Like
  while loops \bold{xIterate} is potentially
  non-terminating, and \bold{Return( )} must be called to
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
}
\seealso{
Other short_circuiting_functions: \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xFold}},
\code{\link{xFoldl}}, \code{\link{xFoldl...}};
\code{\link{xReducer}}, \code{\link{xReducer...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xScan}}, \code{\link{xScan...}},
\code{\link{xScanl}}, \code{\link{xScanl...}}
}

