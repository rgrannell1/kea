
message("xUnion")

test_that("xUnion", {

	expect_that(
		xUnion(list(), list()),
		equals(list()) )
	expect_that(
		xUnion(list(1), list()),
		equals(list(1)) )
	expect_that(
		xUnion(list(1), list(2)),
		equals(list(1, 2)) )
	expect_that(
		xUnion(list(1), list(2, 3)),
		equals(list(1, 2, 3)) )

})
