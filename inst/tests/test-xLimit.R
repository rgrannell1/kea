
context("xLimit")

test_that("xLimit", {
	
	f <- xLimit(2, function (x) x)
	expect_that(f(1), equals(1))
	expect_that(f(1), equals(1))
	expect_that(f(1), equals(Null))

})
