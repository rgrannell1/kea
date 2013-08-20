
context('xSplit')

test_that('xSplit', {

	expect_equal(xSplit('', 'ab'), c('a', 'b'))
	expect_equal(xSplit('.', 'ab'), c('', ''))
	expect_equal(xSplit('a', 'bacd'), c('b', 'cd'))
	
})
