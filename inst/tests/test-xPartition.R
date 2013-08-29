
context('xPartition')

test_that('xPartition', {

	isOdd <- function (x) {
		x %% 2 == 0
	}

	expect_that(
		xPartition(Truth, list()),
		equals( list() )

	expect_that(
		xPartition(Truth, list(1, 2, 3)),
		equals( list(list(1, 2, 3), list()) ))

	expect_that(
		xPartition(Falsity, list(1, 2, 3)),
		equals( list(list(), list(1, 2, 3)) ))

	expect_that(
		xPartition(isOdd, list(1, 2, 3, 4)),
		equals( list(list(2, 4), list(1, 3)) ))

})
