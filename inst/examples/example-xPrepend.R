

# 1. generate the fibonacct numbers under 100.
#
# prepend to make accessing the previous two numbers easy.

fibs <-
	x__(1, 1) $
	xIterate(nums := {

		if (xFirstOf(nums) > 100) {
			Return(xSelect(x. < 100, nums))
		} else {
			xPrepend(xFirstOf(nums) + xSecondOf(nums), nums)
		}

	}) $
	x_Reverse()

# list(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
