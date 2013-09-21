
context("xUnfoldl")

test_that("xUnfoldl", {

	expect_that(
		xUnfoldl(
			function (x) length(x) < 3,
			function (x) c(x, list(1)),
			list()
		),
		equals())

	expect_that(
		xUnfoldl(
			function (n) n < 10,
			function (n) c(n, n + 1),
			1),
		equals(1:10))

})
