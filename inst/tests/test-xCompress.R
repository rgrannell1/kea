
context("xCompress")

test_that("xCompress", {

	expect_that( xCompress(list()), equals(list()) )
	expect_that( xCompress( list(1, list()) ), equals(list(1)) )
	expect_that(
		xCompress( list(1, list(), NULL, integer(0)) ), 
		equals(list(1)) )

})
