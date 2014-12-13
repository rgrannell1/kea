
# 1. HackerRank Functional Programming
#    Given an input list repeat each element n times

# In this problem each element of a list must be repeated n times;
# so the list

# list(1, 2, 3)

# with n = 2 would be become

# list(1, 1, 2, 2, 3, 3)

# I think xFlatMap is the most elegant way of expressing this
# problem, but (as is usually true) xFold can also be used here.
# xFlatMap is like xMap, but it joins its return values end-to-end;
# for this reason it can return an output collection longer than its
# input collection.

repeat_elements <- (n : coll) := {
	xFlatMap(xRepeat(n), coll)
}

# Here we mapped a partially-applied version of the xRepeat function
# (so it repeats its element n times) over the elements we want to
# repeat; xFlatMap then joins them end-to-end as we need.

repeat_elements(2, c(1, 2, 3))

# list(1, 1, 2, 2, 3, 3)


# 2. Factorions
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
#
#    For CRAN's sake, I will only find the first three factorions.

digit_factorial_sum_of <-
	paste %then% xToChars %then% as.integer %then% factorial %then% sum

x_(5000)     $
xIndicesTo() $
xFlatMap(num := {

	digit_factorial_sum <- digit_factorial_sum_of(num)

	if (digit_factorial_sum == num) {
		digit_factorial_sum
	}

})
