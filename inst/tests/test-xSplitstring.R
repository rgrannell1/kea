
context('xSplitstring')

test_that('xSplitstring', {

	expect_equal(xSplitstring('', 'ab'), c('a', 'b'))
	expect_equal(xSplitstring('.', 'ab'), c('', ''))
	expect_equal(xSplitstring('a', 'bacd'), c('b', 'cd'))
	
})
