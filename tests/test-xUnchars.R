
message('xUnchars')

test_that('xUnchars', {

	expect_equal(xUnchars(''), '')
	expect_equal(xUnchars(c('a', 'b')), 'ab')
	expect_equal(xUnchars(c('a', 'bc')), 'abc')

})


