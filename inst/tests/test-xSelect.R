
context("xSelect")

false_as_empty <- function (coll) {
	identical(xSelect(Falsity, coll), list())
}
true_as_identity <- function (coll) {
	identical(xSelect(Truth, coll), coll)
}
na_as_empty <- function (coll) {
	identical(xSelect(Mu, coll), list())
}

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


