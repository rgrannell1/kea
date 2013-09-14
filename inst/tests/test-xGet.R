
context("xGet")

test_that("xGet", {

	expect_that(
		xGet('a')(list(1)),
		equals(list()) )

	expect_that(
		xGet('a')(list(a = 1)),
		equals(list(1)) )

})
