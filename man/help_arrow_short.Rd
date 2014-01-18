\name{help_arrow_short}
\alias{help_arrow_short}
\title{Usage - Short Circuiting Folds}
\description{
Usage - Short Circuiting Folds
}
\details{
The fold family of functions normally iterate over their
entire input collection. For example, the call

\code{xReduce('+', 1:100)}

needs to operate on all 100 elements of its input
collection to obtain its total sum. Not every fold needs to
inspect every element in its input collection to obtain a
sensible result.

\code{ any_nonnegative <- nums := {\cr

xReduce(\cr (any_found : num) := {\cr any_found || num >
0\cr },\cr nums\cr )\cr }\cr }

\code{any_nonnegative( c(-2, -1, 1, rep(10, 1000)) )}
\code{TRUE}

The function any_nonnegative encounters its first
non-negative result after inspecting three elements, but it
will have to waste time iterating over the next 1,000
elements before it returns true. Arrow has a solution to
this problem; the Return( ) function.

\code{ any_nonnegative <- nums := {\cr

xReduce(\cr (any_found : num) := {\cr if (num > 0) {\cr
Return (TRUE)\cr }\cr },\cr nums\cr )\cr }\cr }

\code{any_nonnegative( c(-2, -1, 1, rep(10, 1000)) )}
\code{TRUE}

The Return( ) function short-circuits the Reduce function
immediately after a non-negative value is encountered. In
the above case true is returned after only three
iterations, instead of thousands.
}

