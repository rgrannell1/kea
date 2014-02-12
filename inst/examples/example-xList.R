
# 1. select prime numbers

primes <- upper := {

	is_prime <- num := {
	    if (num == 2) True else min(num %% 2:(num - 1)) > 0
	}

	xList[num, num <- 1:upper, is_prime(num)]
}

primes(17)

# list(2, 3, 5, 7, 11, 13, 17, 19)

# 2.