
context("xDropWhile")

test_that("xDropWhile", {

	isEven <- function (x) {
		x %% 2 == 0
	}

	expect_that(
		xDropWhile(Truth, list(1, 2, 3, 4)),
		equals(list()) )

	expect_that(
		xDropWhile(Falsity, list(1, 2, 3)),
		equals(list(1, 2, 3)) )

	expect_that(
		xDropWhile(isEven, list(2, 4, 1, 3, 4)),
		equals(list(1, 3, 4)) )

	expect_that(
		xDropWhile(function (x) x < 3, list(1, 2, 3, 4, 5)),
		equals(list(3, 4, 5)) )
})
