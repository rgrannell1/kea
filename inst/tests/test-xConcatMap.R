
context("xConcatMap")

test_that("xConcatMap", {

	expect_that(
		xConcatMap(identity, list()),
		equals(list())
	)

	expect_that(
		xConcatMap(function (x) list(x+1), 1:3),
		equals(list(2, 3, 4))
	)
})
