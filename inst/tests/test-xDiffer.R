
context("xDiffer")

test_that("xDiffer", {

	expect_that(
		xDiffer(list(), list()),
		equals(list()) )

	expect_that(
		xDiffer(1:5, list()),
		equals(as.list(1:5)) )

	expect_that(
		xDiffer(1:6, c(2, 4, 6)),
		equals(list(1, 3, 5)) )

})

