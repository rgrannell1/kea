
# 1. CodeEval - Sum the first 1,000 primes
#    This is trivial to do with a fold, but interesting
#    to do with iterate and take,

prime_density <- num := {
	num / ln(num)
}

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
	min(num %% 2:(num - 1)) > 0
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

prime_sum <- x_(1:upper_bound) $ xSelect(is_prime) $ xTake(1000) $ xReduce('+')

