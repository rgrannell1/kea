
context('xSplitAt')

test_that('xSplitAt', {

	expect_equal( xSplitAt(0, 1:4), list(list(), as.list(1:4)) )
	expect_equal( xSplitAt(100, 1:4), list(as.list(1:4), list()) )
	expect_equal( xSplitAt(2, 1:4), list(as.list(1:2), as.list(3:4)) )

})
