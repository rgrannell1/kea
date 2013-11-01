
message('xSplit')

test_that('xSplit', {

	expect_equal( xSplit(0, 1:4), list(list(), as.list(1:4)) )
	expect_equal( xSplit(100, 1:4), list(as.list(1:4), list()) )
	expect_equal( xSplit(2, 1:4), list(as.list(1:2), as.list(3:4)) )

})
