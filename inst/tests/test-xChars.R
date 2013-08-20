
context('xChars')

test_that('xChars', {

	expect_equal(xChars(''), '')
	expect_equal(xChars('ab'), c('a', 'b'))
	expect_equal(xChars('a\nb'), c('a', '\n', 'b'))
	
})
