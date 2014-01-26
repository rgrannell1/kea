\name{xIterate}
\alias{xIterate}
\title{xIterate}
\usage{
xIterate(fn, val)
}
\arguments{
  \item{fn}{a function. The function to repeatedly apply to
  an initial value.}

  \item{val}{an arbitrary value. The initial value to
  iteratively modify.}
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

# 1.
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

# 2. CodeEval - Sum the first 1,000 primes
#    This is trivial to do with a fold, but interesting
#    to do with iterate and take,

# according to the prime number theorem,
# the number of primes is roughly
#
# y <- x / ln(x)
#
# we know y is 1000, but we want to know how many
# integers in 1...n we need to examine to find a thousand
# primes. An analytic function is no fun, so lets
# use a genetic algorithm to find the correct area to search.
#
# We have to make sure to overestimate the search space,
# since the prime density function has a certain error range.
# Otherwise we might find less than 1000 primes.
#
# In this case we use the sqrt(n) approximation to add
# an approximate error to the estimated search space.
#

is_prime <- num := {
    min(num \%\% 2:(num - 1)) > 0
}

prime_density <- x := {
    x / log(x)
}

upper_bound <-
x_(100000L) $
xIterate(
    estimate := {

        mutants <- estimate + c(
            +(estimate / 1000),
            -(estimate / 1000))

        remaining_error <- c(
            abs(prime_density(mutants[1]) - 1000),
            abs(prime_density(mutants[2]) - 1000))

        new_estimate <- mutants[which.min(remaining_error)]

        if (min(remaining_error) < 0.01) {
            Return(new_estimate)
        } else {
            new_estimate
        }
    }
) $
x_Tap(
    upper := {
        floor(sqrt(upper) + upper)
    }
)

# the sum of the first thousand primes.

prime_sum <-
x_(1:upper_bound) $
xSelect(is_prime) $
xTake(1000) $
xReduce('+')
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

