
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

x_(1:50000) $
xMap(
	# a very big anonymous function!
	paste %then% xToChars %then% as.integer %then% factorial %then% sum
) $
xMapIndexed(
	(num : ith) := {
		if (num == ith) num else integer(0)
	}
) $
x_Pack()
