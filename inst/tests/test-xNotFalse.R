
context('xNotFalse')

test_that('xNotFalse', {

	expect_equal(xNotFalse(True), True)
	expect_equal(xNotFalse(False), False)
	expect_equal(xNotFalse(Na), True)

	expect_equal(xNotFalse(c(True, Na)), c(True, True))
	expect_equal(xNotFalse("cat"), True)

})
