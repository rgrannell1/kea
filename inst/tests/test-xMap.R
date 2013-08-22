
context("xMap")

test_that("xMap", {

	expect_equal(xMap(mean, list()), list())
	expect_equal(xMap(identity, 1:3), list(1, 2, 3))
	expect_equal(xMap(function (x) x * x, 1:3), list(1, 4, 9))

})
