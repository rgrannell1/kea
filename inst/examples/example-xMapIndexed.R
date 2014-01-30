
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
	paste %then% xToChars %then% as.integer %then% factorial %then% sum

x_(1:50000) $
xFlatMap(num := {

	digit_factorial_sum <- digit_factorial_sum_of(num)

	if (digit_factorial_sum == num) {
		digit_factorial_sum
	}

})
