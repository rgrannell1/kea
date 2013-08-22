
context("xCount")

test_that("xCount", {

	expect_that(
		xCount(Truth, list()),
		equals(0)
	)
	expect_that(
		xCount(Truth, 1:4),
		equals(4)
	)
	expect_that(
		xCount(Falsity, 1:4),
		equals(0)
	)
	expect_that(
		xCount(function (x) x %% 2 == 0, 1:4),
		equals(2)
	)

})
