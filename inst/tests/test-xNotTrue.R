
context('xNotTrue')

test_that('xNotTrue', {

	expect_equal(xNotTrue(True), False)
	expect_equal(xNotTrue(False), True)
	expect_equal(xNotTrue(Na), True)

	expect_equal(xNotTrue(c(True, Na)), c(False, True))
	expect_equal(xNotTrue("cat"), True)

})
