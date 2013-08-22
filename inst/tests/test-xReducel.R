
context("xReducel")

test_that("xReducel", {

	expect_that(
		xReducel("c", 1:10),
		equals(1:10))

	expect_that(
		xReducel("+", 1:10),
		equals(55))

	expect_that(
		xReducel(max, 0:10),
		equals(10)
	)
})
