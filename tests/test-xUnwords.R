
message('xUnwords')

test_that('xUnwords', {

	expect_equal(xUnwords(''), '')
	expect_equal(xUnwords(c('a', 'b')), 'a b')
	expect_equal(xUnwords(c('a', 'bc')), 'a bc')

})


