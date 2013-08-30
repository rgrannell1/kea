
context("xSetProd(")

test_that("xSetProd(", {

	expect_that( xSetProd(1:3, list()), equals(list()) )
	expect_that(
		xSetProd(1:2, 1:2),
		equals(list( list(1, 1), list(2, 1), list(1, 2), list(2, 2) )) )
	expect_that(
		xSetProd(1:3, 1),
		equals(list( list(1, 1), list(2, 1), list(3, 1) ))
	)

})
