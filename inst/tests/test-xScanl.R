
context("xScanl")

test_that("xScanl", {

	expect_that(
		xScanl("+", 0, list()),
		equals(0)
	)

	expect_that(
		xScanl("+", 0, 1:10), 
		equals( as.list(cumsum(0:10)) )
	)

})