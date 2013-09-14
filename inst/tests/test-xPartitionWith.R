
context('xPartitionWith')

test_that('xPartitionWith', {

	isOdd <- function (x) {
		x %% 2 == 0
	}

	expect_equal(xPartitionWith(Truth, list()), list())

	expect_equal(
		xPartitionWith(Truth, list(1, 2, 3)),
		list(list(1, 2, 3), list()) )

	expect_equal(
		xPartitionWith(Falsity, list(1, 2, 3)),
		list(list(), list(1, 2, 3)) )

	expect_equal(
		xPartitionWith(isOdd, list(1, 2, 3, 4)),
		list(list(2, 4), list(1, 3)) )

})
