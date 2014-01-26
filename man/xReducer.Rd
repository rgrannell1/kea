\name{xReducer}
\alias{xReducer}
\alias{xReducer...}
\title{xReducer}
\usage{
xReducer(fn, coll)

xReducer...(fn, ...)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \bold{fn} can later take as its left argument.}

  \item{coll}{a collection. The collection to reduce to a
  single value.}

  \item{...}{see above.}
}
\value{
An arbitrary value, depending on the function \bold{fn}.
}
\description{
Successively combine a list of values into a single value
using a binary function (right to left).
}
\details{
Fold successively combines the first two elements of a
collection into one element, until only one element is
remaining. The combining is done with with a binary
combining function. Examples of such functions - functions
that take two elements and combine them into one element -
include the plus operator and list concatenation.

\bold{Associative vs Non-Associative Values.}

There are two important types of combining functions:
associative and non-associative. The difference between
these functions is best demonstrated using infix notation.
An example of an associative function is the plus operator,
because the following equation holds

\code{(a + b) + c == a + (b + c)}

for arbitrary numbers a, b, c. Bracketing from the left or
the right does not alter the result of adding several
numbers together. The below example involves using
\bold{xFoldl} to add a vector of numbers. \bold{xFoldl}
successively combines (with +) the leftmost two values into
a single number, and repeats the process until the sum of
the entire list is found.

\code{xFoldl('+', 0, 1:5)}

\code{(((((0 + 1) + 2) + 3) + 4) + 5)}

\code{0 + 1 + 2 + 3 + 4 + 5} \code{1 + 2 + 3 + 4 + 5}

\code{3 + 3 + 4 + 5}

\code{6 + 4 + 5}

\code{10 + 5}

\code{15}

As stated above the plus operator is associative, so both
\bold{xFoldl} or \bold{xFoldr} yield the same result.

Non-associative combining functions often focus on
accumulating a data structure entirely different from the
input collection, one element at a time. In the following
example a non-associative combining function takes an tally
list that counts votes for bsd versus linux as its left
argument, and a vote as its right argument. If the vote is
for bsd or linux the tally list is updated, if not it is
returned as is.

\code{combiner <- (acc : elem) := if (elem in names(acc))
elem[[acc]] + 1 else acc}

\code{votes <- c('linux', 'mac', 'mac', 'bsd', 'windows',
'linux')}

\code{xFoldl(combiner, list(linux = 0, bsd = 0), votes) }

The votes are tallied in the following order:

\code{list(linux = 0, bsd = 0), 'linux', 'mac', 'mac',
'bsd', 'windows', 'linux'}

\code{list(linux = 1, bsd = 0), 'mac', 'mac', 'bsd',
'windows', 'linux'}

\code{list(linux = 1, bsd = 0), 'mac', 'bsd', 'windows',
'linux'}

\code{list(linux = 1, bsd = 0), 'bsd', 'windows', 'linux'}

\code{list(linux = 1, bsd = 1), 'windows', 'linux'}

\code{list(linux = 2, bsd = 1), 'linux'}

\code{list(linux = 3, bsd = 1)}

The combining function is non-associative, since it
increments a field in its left argument, while the right
argument doesn't even have fields.

\bold{Short-Circuiting}

The folds examined so far involve iterating over an entire
input collection. When the input collections are very long
this isn't always desirable. Sometimes it is possible - an
more efficent - to return a result before checking every
element of the input collection.

A good demonstration of short circuiting is to improve the
efficiency of searching for a value. For example, if we
want to check if a collection contains any na value we
could (but shouldn't) use the following inefficient code.

\code{coll <- c(1, 2, 3, NA, rep(10, 1000))}

\code{xFoldl((acc : elem) := acc || is.na(elem), False,
coll)}

\code{True} x The obvious problem is the fourth element of
the very long collection is na, but the code searches
though thousands of elements after finding a match. If we
were searching the collection with a for loop we would use
\bold{break} or \bold{return( )} to terminate the search
early and return true. Arrow adds a similar construct for
terminating certain higher-order functions called the
\bold{Return( )} function.

This function can be used to break out of the fold early;
in this case upon discovery of an na value.

\code{xFoldl((acc : elem) := if (is.na(elem)) Return(True)
else False, False, coll)}

The use of \bold{Return( )} reduces the number of steps
required to return a result from thousands to less than
ten.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero, and
  returns the value inside \bold{coll} if coll is
  length-one.
}
\seealso{
Other folding_functions: \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xFold}},
\code{\link{xFoldl}}, \code{\link{xFoldl...}};
\code{\link{xReduce}}, \code{\link{xReduce...}},
\code{\link{xReducel}}, \code{\link{xReducel...}};
\code{\link{xScan}}, \code{\link{xScan...}},
\code{\link{xScanl}}, \code{\link{xScanl...}}

Other short_circuiting_functions: \code{\link{xFoldr}},
\code{\link{xFoldr...}}; \code{\link{xFold}},
\code{\link{xFoldl}}, \code{\link{xFoldl...}};
\code{\link{xIterate}}; \code{\link{xReduce}},
\code{\link{xReduce...}}, \code{\link{xReducel}},
\code{\link{xReducel...}}; \code{\link{xScan}},
\code{\link{xScan...}}, \code{\link{xScanl}},
\code{\link{xScanl...}}
}

