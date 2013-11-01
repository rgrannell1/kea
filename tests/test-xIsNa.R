
message('xIsNa')

test_that('xIsNa', {

	expect_equal(xIsNa(True), False)
	expect_equal(xIsNa(False), False)
	expect_equal(xIsNa(Na), True)

	expect_equal(xIsNa(c(True, Na)), c(False, True))

})
