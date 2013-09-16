
context("xConcat")

test_that("xConcat", {

	expect_equal(
		xConcat( list() ),
		list())

	expect_equal(
		xConcat( list(1), c(2, 3) ),
		list(1, 2, 3))

	expect_equal(
		xConcat( list(), pairlist(), list(1, 2) ),
		list(1, 2))
})
