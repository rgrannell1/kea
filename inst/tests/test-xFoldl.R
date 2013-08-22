
context("xFoldl")

test_that("xFoldl", {

	expect_that(
		xFoldl("c", 0, 1:10),
		equals(0:10))

	expect_that(
		xFoldl("+", 0, 1:10),
		equals(55))

	expect_that(
		xFoldl("+", 0, list()),
		equals(0)
	)

	expect_that(
		xFoldl(
			function (a, b) c(a, list(elem = b)), 
			list(), 
			c(1, 2, 3)),
		equals(list(elem = 1, elem = 2, elem = 3))
	)

})
