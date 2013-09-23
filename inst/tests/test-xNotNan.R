
context('xNotNan')

test_that('xNotNan', {

	expect_equal(xNotNan(True), True)
	expect_equal(xNotNan(False), True)
	expect_equal(xNotNan(NaN), False)

	expect_equal(xNotNan(c(True, NaN)), c(True, False))
	expect_equal(xNotNan("cat"), True)

})
