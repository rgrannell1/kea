
message('xCollapse')

test_that('xCollapse', {

	expect_equal(xCollapse('', ''), '')
	expect_equal(xCollapse('', c('a', 'b')), 'ab')
	expect_equal(xCollapse('|', c('a', 'bc')), 'a|bc')

})
