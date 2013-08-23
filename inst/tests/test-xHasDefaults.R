
context("xHasDefaults")

test_that("xHasDefaults", {

	expect_equal(
		xHasDefaults(function (a, b) a + b),
		c(False, False)
	)
	expect_equal(
		xHasDefaults(function (a, b = True) a + b),
		c(False, True)
	)
	expect_equal(
		xHasDefaults(function (a = True, ...) a + b),
		c(True, False)
	)
	expect_equal(
		xHasDefaults(function () {}),
		logical(0)
	)

})
