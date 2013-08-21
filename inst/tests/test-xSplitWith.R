
context("xSplitWith")

test_that("xSplitWith", {

	isEven <- function (x) {
		x %% 2 == 0
	}

	expect_that(
		xSplitWith(Truth, list()), 
		equals( list(list(), list()) ))
	
	expect_that(
		xSplitWith(Truth, list(1,2,3)), 
		equals( list(list(1, 2, 3), list()) ))

	expect_that(
		xSplitWith(Falsity, list(1,2,3)), 
		equals( list(list(), list(1, 2, 3)) ))

	expect_that(
		xSplitWith(isEven, list(2, 4, 6, 7, 8, 9, 10)),
		equals( list(list(2, 4, 6), list(7, 8, 9, 10)) )
	)

})
