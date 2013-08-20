
context('xLines')

test_that('xLines', {

	expect_equal(xLines(''), character(0)) # for now...
	expect_equal(xLines('ab'), 'ab')
	expect_equal(xLines('a\nb'), c('a', 'b'))
	expect_equal(xLines('b\ncd'), c('b', 'cd'))
	
})
