
context('xFirst')

test_that('xFirst', {

	expect_error( xFirst(list()) )
	expect_equal( xFirst(list(1)), 1)
	expect_equal( xFirst(list(1, 2)), 1)
})
