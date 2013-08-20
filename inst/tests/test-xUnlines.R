
context('xUnlines')

test_that('xUnlines', {

	expect_equal(xUnlines(''), '')
	expect_equal(xUnlines(c('a', 'b')), 'a\nb')
	expect_equal(xUnlines(c('a', 'bc')), 'a\nbc')

})
