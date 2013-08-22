
context('xInit')

test_that('xInit', {

	expect_equal( xInit(list(1)), list())
	expect_equal( xInit(list(1, 2)), list(1))
	expect_equal( xInit(list(1, 2, 3)), list(1, 2))

})
