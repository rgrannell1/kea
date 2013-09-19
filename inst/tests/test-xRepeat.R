
context("xRepeat")

test_that("xRepeat", {

	expect_that(
		xRepeat(1:10, 0), equals(list()) )
	expect_that(
		xRepeat(1:10, 1), equals(as.list(1:10)) )
	expect_that(
		xRepeat(1:5, 2), equals( as.list(c(1:5, 1:5)) ) )

})

