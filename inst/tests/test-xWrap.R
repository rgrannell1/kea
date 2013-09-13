
context("xWrap")

test_that("xWrap", {

	expect_that(
		xWrap(identity, function (wraps, x) identity(x + 1))(1),
		equals(2) )

	expect_that(
		xWrap('+', function (wraps, ...) wraps(...) + 1)(1, 1),
		equals(3) )

})
