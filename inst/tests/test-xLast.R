
context('xLast')

test_that('xLast', {

	expect_error( xLast(list()) )
	expect_equal( xLast(list(1)), 1)
	expect_equal( xLast(list(1, 2)), 2)
})
