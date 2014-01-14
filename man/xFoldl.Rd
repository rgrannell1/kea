\name{xFoldl}
\alias{xFold}
\alias{xFoldl}
\alias{xFoldl...}
\title{xFoldl}
\usage{
xFoldl(fn, val, coll)

xFold(fn, val, coll)

xFoldl...(fn, val, ...)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its left argument.}

  \item{val}{an arbitrary value.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
An arbitrary value, depending on the function \code{fn}.
}
\description{
Successively combine a list of values into a single value
using a binary function (left to right, with an initial
value).
}
\section{Corner Cases}{
  returns \code{val} if \code{coll} is length-zero.
}

\section{Variadic Functions

\link{ help_variadic }}{

}

\section{Appendix-Short-Circuiting}{
  The fold family of functions include an optional
  mechanism of prematurely returning an answer, similar to
  the \code{return()} function in base R. By calling
  \code{Return} with the value you want \code{fn} to yield
  instantly, the fold/reduce is halted, and that value is
  yielded as the result of that proceedure.

  \code{Return()} essentially acts as a higher-order return
  function, that breaks "further" than the normal return
  statement.

  \code{to_search <- c(list(0, 1, 0, 3, 2, 10), rep(4,
  1000))}

  \code{ xFoldl(\cr (checked : searched ) := {\cr if
  (searched == 10) Return(checked) else checked + 1\cr
  },\cr list(),\cr to_search\cr ) }

  \code{6}

  In this case, the Foldl returns its results prematurely
  after 6 searches, as opposed to the thousands of
  iterations that the base function \code{Reduce()} would
  have to preform before returning its result. By using
  \code{Return()} where possible folding functions can be
  made to preform significantly bettern
}
\examples{

# 1. sum a vector with fold.

xFold("+", 0, 1:10)

55

# 2. demonstrate how Return works by using
#    it to break out of the fold across 26 letters
#    prematurely.

xFold(
    (number_checked : letter) := {

        if (letter == 'q') {
            Return (number_checked)
        } else {
            number_checked + 1
        }
    },
    0,
    letters
)

# terminated after 16 checks, not 26.
16

# 3. HackerRank Functional Programming #4
#    Return the sum of the odd elements of a list

# There are two parts to this problem; selecting the odd numbers from a list
# and then summing them. xSelect is (naturally) the solution to the latter half.
# xFold takes a binary function (in this case 'plus'), and successively merges values (from
# left to right) in a collection until a single result is returned.

# Arrow chaining methods work nicely for multi-part processing of data.

sum_odds <- nums := {

    x_(nums)$
    xSelect(
        x := {x \%\% 2 == 1}
    )$
    x_Fold('+', 0)
}

sum_odds(c(1, 2, 3, 4, 5, 6))

9

# A more efficient solution would only use xFold; the right argument is checked to see
# if it is odd; if it is it's added to the accumulator value.

sum_odds <- nums := {

    xReduce(
        (acc : nextelem) := {
            if (nextelem \%\% 2 == 1) {
                acc + nextelem
            } else {
                acc
            }
        },
        0,
        nums
    )
}

sum_odds(c(1, 2, 3, 4, 5, 6))

9

# 4. HackerRank Functional Programming
#    Remove the odd indices from a collection

# As is normal for functional programming, fold is the solution to all problems.
# Most functions in Arrow could actually be expressed by partially applying fold,
# but that would be inefficient.

#
# In this case we want to take a collection xs and keep xs[ 2, 4, 6, ... ]
# We can use xFold to take each index of the collection and only accumulate
# the values at even indices.

remove_odds <- coll := {

    xFold(
        (acc : index) := {
            if (index \%\% 2 == 0) {
                c(acc, coll[index])
            } else {
                acc
            }
        },
        xUnit(coll),
        seq_along(coll)
    )

}

remove_odds(letters)

c("b", "d", "f", "h", "j", "l", "n", "p", "r", "t", "v", "x", "z")

# 5. HackerRank Functional Programming
# Area under curves and solids of rotation

# Time to flex my atrophied Calculus muscles...

# This particular problem involves finding the area underneath a
# subsection of a polynomial of x, and finding the area of the solid of
# rotation about the x-axis. Luckily numerical integration isn't so hard.

# The easiest way to approximate the area under a curve is to approximate the
# curve with small rectangles, and to summate their area. This isn't a great
# approximation -
# it converges on the true value slowly with respect to rectangle width -
# but it will do here.

# I think this is most naturally expressed with xFold; starting from an initial
# area of 0 add the area of each rectangle under the polynomial to the accumulated area.

integrate <- (f : x_bounds : epsilon ) := {

    x_values <- seq(from = x_bounds[1], to = x_bounds[2], by = epsilon)

    xFold(
        (area : x) := {
            area + (f(x) * epsilon)
        },
        0,
        x_values
    )
}

integrate(
    x := x^3 - 9*x^2,
    c(-10, +10),
    0.001
)

-6000.9

# Wolfram Alpha (where most of my calculus knowledge comes from)
# says -6000, so not bad.
}
\seealso{
Other folding_functions: \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xReducer}},
\code{\link{xReducer...}}; \code{\link{xReduce}},
\code{\link{xReduce...}}, \code{\link{xReducel}},
\code{\link{xReducel...}}; \code{\link{xScan}},
\code{\link{xScan...}}, \code{\link{xScanl}},
\code{\link{xScanl...}}
}

