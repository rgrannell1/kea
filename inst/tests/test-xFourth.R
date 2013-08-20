
context('xFourth')

test_that('xFourth', {

	expect_error( xFourth(list(1, 2, 3)) )
	expect_equal( xFourth(list(1, 2, 3, 4)), 4)
	expect_equal( xFourth(list(1, 2, 3, 4, 5)), 4)
})
