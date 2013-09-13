
context("xCombinations")

test_that("xCombinations", {

	expect_equal(
		xCombinations(0, 1:3), list() )

	expect_equal(
		xCombinations(1, 1:3),
		list(list(1), list(2), list(3)) )

	expect_equal(
		xCombinations(2, 1:3),
		list(list(1L, 2L), list(1L, 3L), list(2L, 3L)) )
})
