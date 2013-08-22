
context("xReducer")

test_that("xReducer", {

	expect_that(
		xReducer("c", 10:1),
		equals(1:10))

	expect_that(
		xReducer("+", 1:10),
		equals(55))

	expect_that(
		xReducer(max, 0:10),
		equals(10)
	)
})
