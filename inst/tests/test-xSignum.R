
context('xSignum')

test_that('xSignum', {

	expect_equal( xSignum(integer(0)), integer(0))
	expect_equal( xSignum(c(-2, 0, 2)), c(-1, 0, 1))
	expect_equal( xSignum(list(-1, 0, 1)), c(-1, 0, 1))

})
