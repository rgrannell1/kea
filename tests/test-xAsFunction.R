
message("xAsFunction")

test_that("xAsFunction", {

	expect_that( xAsFunction( list() )(0), equals(list()) )
	expect_that( xAsFunction( list(1) )(1), equals(list(1)) )
	expect_that( xAsFunction( list(1, 3) )(2), equals(list(3)) )
	expect_that( xAsFunction( list(1, 3, 4) )(1, 3), equals(list(1, 4)) )

})
