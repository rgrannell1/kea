
context("xReject")

test_that("xReject", {

	odds <- function (x) {
		x %% 2 == 0
	}

	expect_that(
		xReject(Truth, pairlist()), equals(list()) )
	expect_that(
		xReject(Truth, list(1, 2, 3)), equals(list()) )
	expect_that(
		xReject(Falsity, list(1, 2, 3)), equals(list(1, 2, 3)) )
	expect_that(
		xReject(odds, as.list(1:6)), equals(list(1, 3, 5)) )
	expect_that(
		xReject(odds, 1:6), equals(list(1, 3, 5)) )
	
})
