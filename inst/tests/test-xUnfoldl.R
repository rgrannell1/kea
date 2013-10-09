
context("xUnfoldl")

test_that("xUnfoldl", {

	expect_that(
		xUnfoldl(
			function (n) n < 10,
			function (n) c(n, n + 1),
			1),
		equals(as.list(1:10)) )

})
