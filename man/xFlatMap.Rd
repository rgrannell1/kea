\name{xFlatMap}
\alias{xFlatMap}
\alias{xFlatMap...}
\title{xFlatMap}
\usage{
xFlatMap(fn, coll)

xFlatMap...(fn, ...)
}
\arguments{
  \item{fn}{a unary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Concatenate the results of applying a function to each
element of a collection.
}
\section{Corner Cases}{
  Returns the empty list if \bold{coll} is length-zero.
}
\examples{


# 1. HackerRank Functional Programming
#    Given an input list repeat each element n times

# In this problem each element of a list must be repeated n times;
# so the list

list(1, 2, 3)

# with n = 2 would be become

list(1, 1, 2, 2, 3, 3)

# I think xFlatMap is the most elegant way of expressing this problem, but (as is usually true)
# xFold can also be used here. xFlatMap is like xMap, but it joins its return values end-to-end;
# for this reason it can return an output collection longer than its input collection.

repeat_elements <- (n : coll) := {

	xFlatMap(
		xPartial...(xRepeat, num = n),
		coll)

}

# Here we mapped a partially-applied version of the xRepeat function (so it repeats its element n times) over
# the elements we want to repeat; xFlatMap then joins them end-to-end as we need.

repeat_elements(2, c(1, 2, 3))

list(1, 1, 2, 2, 3, 3)
}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xMapIndexed}},
\code{\link{xMapIndexed...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

