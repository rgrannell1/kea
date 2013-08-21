
context('xSplitstr')

test_that('xSplitstr', {

	expect_equal(xSplitstr('', 'ab'), c('a', 'b'))
	expect_equal(xSplitstr('.', 'ab'), c('', ''))
	expect_equal(xSplitstr('a', 'bacd'), c('b', 'cd'))
	
})
