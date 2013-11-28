
message('xIsNan')

test_that('xIsNan', {

	expect_equal(xIsNan(NaN), True)
	expect_equal(xIsNan(1), False)
	expect_equal(xIsNan(Na), False)

	expect_equal(xIsNan(c(True, NaN)), c(False, True))

})
