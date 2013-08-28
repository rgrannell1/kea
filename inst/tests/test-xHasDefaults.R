
context("xHasDefaults")

test_that("xHasDefaults", {

	expect_equal(
		xHasDefaults(function (a, b) a + b),
		c(a = False, b = False)
	)
	expect_equal(
		xHasDefaults(function (a, b = True) a + b),
		c(a = False, b = True)
	)
	expect_equal(
		xHasDefaults(function (a = True, ...) a + b),
		c(a = True, ... = False)
	)
	expect_equal(
		xHasDefaults(function () {}),
		logical(0)
	)

})
