
context("xSelect")

test_that("xSelect", {

	odds <- function (x) {
		x %% 2 == 0
	}

	expect_that(
		xSelect(Truth, pairlist()), equals(list()) )
	expect_that(
		xSelect(Truth, list(1, 2, 3)), equals(list(1, 2, 3)) )
	expect_that(
		xSelect(Falsity, list(1, 2, 3)), equals(list()) )
	expect_that(
		xSelect(odds, as.list(1:6)), equals(list(2, 4, 6)) )
	expect_that(
		xSelect(odds, 1:6), equals(list(2, 4, 6)) )
	
})




forall(
	list(
		fn = G$logical_functions,
		coll = G$collection_zero),
	xSelect(fn, coll) %equals% list()
)






