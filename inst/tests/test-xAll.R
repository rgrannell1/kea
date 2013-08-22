
context("xAll")

test_that('xAll', {

	isOdd <- function (x) {
		x %% 2 == 0
	}

	expect_equal(xAll(Truth, list()), True)

	expect_equal(xAll(Truth, 1:3), True)
	expect_equal(xAll(Falsity, 1:3), False)
	expect_equal(xAll(isOdd, 1:10), False)
	expect_equal(xAll(NonApplicability, 1:3), False)
	
})
