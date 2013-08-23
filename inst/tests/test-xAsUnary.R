
context("xAsUnary")

test_that("xAsUnary", {

	expect_equal(xArity(xAsUnary(function (a, b) a + b)), 1)
	expect_equal(xAsUnary(function (a, b) a + b)(list(1, 2)), 3)

})
