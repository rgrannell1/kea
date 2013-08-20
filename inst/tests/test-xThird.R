
context('xThird')

test_that('xThird', {

	expect_error( xThird(list(1, 2)) )
	expect_equal( xThird(list(1, 2, 3)), 3)
	expect_equal( xThird(list(1, 2, 3, 4)), 3)
})
