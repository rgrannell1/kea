
message("xPack")

test_that("xPack", {

	expect_that( xPack(list()), equals(list()) )
	expect_that( xPack( list(1, list()) ), equals(list(1)) )
	expect_that(
		xPack( list(1, list(), Null, integer(0)) ),
		equals(list(1)) )

})
