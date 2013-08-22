
context("xPositionl")

test_that('xPositionl', {

	isEven <- function (n) {
		n %% 2 == 0
	}

	expect_equal(xPositionl(Truth, list()), integer(0))
	expect_equal(xPositionl(Falsity, 1:10), integer(0))
	expect_equal(xPositionl(Truth, 1:10), 1)
	expect_equal(xPositionl(isEven, 1:10), 2)
	
})
