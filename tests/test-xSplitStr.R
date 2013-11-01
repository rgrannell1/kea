
message('xSplitStr')

test_that('xSplitStr', {

	expect_equal(xSplitStr('', 'ab'), c('a', 'b'))
	expect_equal(xSplitStr('.', 'ab'), c('', ''))
	expect_equal(xSplitStr('a', 'bacd'), c('b', 'cd'))

})
