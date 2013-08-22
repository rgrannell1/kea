
context('xSplitString')

test_that('xSplitString', {

	expect_equal(xSplitString('', 'ab'), c('a', 'b'))
	expect_equal(xSplitString('.', 'ab'), c('', ''))
	expect_equal(xSplitString('a', 'bacd'), c('b', 'cd'))
	
})
