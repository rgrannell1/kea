
context("xPositionr")

test_that('xPositionr', {

	isEven <- function (n) {
		n %% 2 == 0
	}

	expect_equal(xPositionr(Truth, list()), integer(0))
	expect_equal(xPositionr(Falsity, 1:10), integer(0))
	expect_equal(xPositionr(Truth, 1:10), 10)
	expect_equal(xPositionr(isEven, 1:10), 10)
	
})
