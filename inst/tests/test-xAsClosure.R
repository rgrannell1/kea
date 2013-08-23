
context("xAsClosure")

test_that("xAsClosure", {

	expect_false( is.primitive(xAsClosure("+")) )
	expect_equal( names( formals(xAsClosure("+")) ), c('e1', 'e2') )
	expect_equal( xAsClosure("+")(1, 2), 3 )

})
