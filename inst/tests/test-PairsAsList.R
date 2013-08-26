
context("xPairsAsList")

test_that("xPairsAsList", {

	expect_equal(
		xPairsAsList( list(list('a', 1), list('b', 2)) ),
		list(a = 1, b = 2))

	expect_equal(
		xPairsAsList( list(list('ab', list(1, 2)), list('b', 2)) ),
		list(ab = list(1, 2), b = 2))

})