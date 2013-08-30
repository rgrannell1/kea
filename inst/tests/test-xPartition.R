
context('xPartition')

test_that('xPartition', {

	isOdd <- function (x) {
		x %% 2 == 0
	}

	expect_equal(xPartition(Truth, list()), list())

	expect_equal(
		xPartition(Truth, list(1, 2, 3)),
		list(list(1, 2, 3), list()) )

	expect_equal(
		xPartition(Falsity, list(1, 2, 3)),
		list(list(), list(1, 2, 3)) )

	expect_equal(
		xPartition(isOdd, list(1, 2, 3, 4)),
		list(list(2, 4), list(1, 3)) )

})
