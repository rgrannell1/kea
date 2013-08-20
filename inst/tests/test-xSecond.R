
context('xSecond')

test_that('xSecond', {

	expect_error( xSecond(list(1)) )
	expect_equal( xSecond(list(1, 2)), 2)
	expect_equal( xSecond(list(1, 2, 3)), 2)
})
