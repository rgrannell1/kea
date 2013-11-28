
message("xTakeWhile")


Truth <-
	function (...) True
Falsity <-
	function (...) False

test_that("xTakeWhile", {

	expect_that(
		xTakeWhile(Falsity, list(1, 2, 3, 4)),
		equals(list()) )

	expect_that(
		xTakeWhile(Truth, list(1, 2, 3)),
		equals(list(1, 2, 3)) )

	expect_that(
		xTakeWhile(Falsity, list(1, 2, 3)),
		equals(list()) )

	expect_that(
		xTakeWhile(function (x) x < 3, list(1, 2, 3, 4)),
		equals(list(1, 2)) )

})
