
context('xWords')

test_that('xWords', {

	expect_equal(xWords(''), '')
	expect_equal(xWords('ab'), 'ab')
	expect_equal(xWords('a b'), c('a', 'b'))
	expect_equal(xWords('b cd'), c('b', 'cd'))
	
})
