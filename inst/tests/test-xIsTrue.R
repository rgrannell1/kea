
context('xIsTrue')

test_that('xIsTrue', {

	expect_equal(xIsTrue(True), True)
	expect_equal(xIsTrue(False), False)
	expect_equal(xIsTrue(Na), False)

	expect_equal(xIsTrue(c(True, Na)), c(True, False))

})
