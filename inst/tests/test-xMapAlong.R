
context("xMapAlong")

test_that("xMapAlong", {

	expect_that(
		xMapAlong(function (x, i) i, 3:1),
		equals(list(1, 2, 3))
	)

	expect_that(
		xMapAlong(function (x, i) x, 3:1),
		equals(list(3, 2, 1))
	)

})
