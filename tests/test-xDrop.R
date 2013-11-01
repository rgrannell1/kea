
message('xDrop')

test_that('xDrop always returns a list, with no out-of-bounds', {

	expect_equal(xDrop(1000, list()), list())
	expect_equal(xDrop(0, 1:10), as.list(1:10))
	expect_equal(xDrop(100, 1:10), list())

})
