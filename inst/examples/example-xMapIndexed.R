
# 1. Factorions
#    factorions are numbers whose value is the sum of the
#    factorials of its digits. For example,
#    145 = 1! + 4! + 5!
#    the below example uses a lot of chained function composition,
#    to process and sum each digit of the number to search,

x_(1:10000) $
xMap(
	sum %of% factorial %of% as.integer %of% xToChars %of% paste) $
xMapIndexed(
	(num : ith) := {
		if (num == ith) num else integer(0)
	}
) $
x_Pack()
