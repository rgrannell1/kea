
context("xRepeat")

test_that("xRepeat", {

	expect_that(
		xRepeat(0, 1:10), equals(list()) )
	expect_that(
		xRepeat(1, 1:10), equals(as.list(1:10)) )
	expect_that(
		xRepeat(2, 1:5), equals( as.list(c(1:5, 1:5)) ) )

})

