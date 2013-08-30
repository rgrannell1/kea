
context("xForall")

test_that("xForall", {

	expect_equal(xForall(identity), True)
	expect_equal(xForall(function (x) x^2 == x, 1:10), False)
	expect_equal(xForall(function (x, y) x+y == y+x, 1:10, 1:10), True)

})
