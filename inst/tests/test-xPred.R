
context('xPred')

test_that('xPred', {

	expect_equal( xPred(1), 0)
	expect_equal( xPred(c(1, 0)), c(0, -1))
	expect_equal( xPred(list(1, 2)), c(0, 1))

})
