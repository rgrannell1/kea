\name{xMapIndexed}
\alias{xMapIndexed}
\alias{xMapIndexed...}
\title{xMapIndexed}
\usage{
xMapIndexed(fn, coll)

xMapIndexed...(fn, ...)
}
\arguments{
  \item{fn}{a binary function.}

  \item{coll}{a collection.}

  \item{...}{see above.}
}
\value{
A list.
}
\description{
Apply a binary function to each element of a collection and
its index.
}
\section{Corner Cases}{
  Returns the empty list is \bold{coll} is length-zero.
}
\examples{

# 1. Factorions
#    factorions are numbers whose value is the sum of the
#    factorials of its digits. For example,
#    145 = 1! + 4! + 5!
#    the below example uses a lot of chained function composition,
#    to process and sum each digit of the number to search,
#
#    There are only four factorions in base 10:
#
#    1, 2, 145, 40585
#
#    There are more efficient ways to find the numbers,
#    but a brute force search is fun, if really slow.

digit_factorial_sum_of <-
	paste \%then\% xToChars \%then\% as.integer \%then\% factorial \%then\% sum

x_(1:50000) $
xFlatMap(num := {

	digit_factorial_sum <- digit_factorial_sum_of(num)

	if (digit_factorial_sum == num) {
		digit_factorial_sum
	}

})
}
\seealso{
Other mapping_functions: \code{\link{xDeepMap}},
\code{\link{xDeepMap...}}; \code{\link{xFlatMap}},
\code{\link{xFlatMap...}}; \code{\link{xMapMany}},
\code{\link{xMapMany...}}; \code{\link{xMapply}},
\code{\link{xMapply...}}; \code{\link{xMap}},
\code{\link{xMap...}}; \code{\link{xVectorise}},
\code{\link{xVectorize}}
}

