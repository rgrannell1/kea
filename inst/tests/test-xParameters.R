
context("xParameters")

test_that("xParameters", {

	expect_equal(xParameters(function () {}), character(0))
	expect_equal(xParameters(function (a) a), "a")
	expect_equal(xParameters(function (a = 1, b) a), c("a", "b"))

})
