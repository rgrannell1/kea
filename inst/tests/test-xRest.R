
context('xRest')

test_that('xRest', {

	expect_equal( xRest(list()), list())
	expect_equal( xRest(list(1)), list())
	expect_equal( xRest(list(1, 2)), list(2))
	expect_equal( xRest(list(1, 2, 3)), list(2, 3))

})
