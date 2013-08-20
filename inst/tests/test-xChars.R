
context('xChars')

test_that('xChars', {

	expect_equal(xChars(''), character(0)) # for now...
	expect_equal(xChars('ab'), c('a', 'b'))
	expect_equal(xChars('a\nb'), c('a', '\n', 'b'))
	
})
