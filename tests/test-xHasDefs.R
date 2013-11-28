
message("xHasDefs")

test_that("xHasDefs", {

	expect_equal(
		xHasDefs(function (a, b) a + b),
		c(a = False, b = False)
	)
	expect_equal(
		xHasDefs(function (a, b = True) a + b),
		c(a = False, b = True)
	)
	expect_equal(
		xHasDefs(function (a = True, ...) a + b),
		c(a = True, ... = False)
	)
	expect_equal(
		xHasDefs(function () {}),
		logical(0)
	)

})
