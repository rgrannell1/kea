
message('xTake')

test_that('xTake always returns a list, with no out-of-bounds', {

	expect_equal(xTake(1000, list()), list())
	expect_equal(xTake(0, 1:10), list())
	expect_equal(xTake(100, 1:10), as.list(1:10))

})
