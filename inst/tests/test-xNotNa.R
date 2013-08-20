
context('xNotNa')

test_that('xNotNa', {

	expect_equal(xNotNa(True), True)
	expect_equal(xNotNa(False), True)
	expect_equal(xNotNa(Na), False)

	expect_equal(xNotNa(c(True, Na)), c(True, False))
	expect_equal(xNotNa("cat"), True)

})
