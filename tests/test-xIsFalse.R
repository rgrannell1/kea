
message('xIsFalse')

test_that('xIsFalse', {

	expect_equal(xIsFalse(True), False)
	expect_equal(xIsFalse(False), True)
	expect_equal(xIsFalse(Na), False)

	expect_equal(xIsFalse(c(True, Na)), c(False, False))

})
