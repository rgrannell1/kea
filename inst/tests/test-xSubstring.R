
context('xSubstring')

test_that('xSubstring', {

	expect_equal(xSubstring('', 0), '')
	expect_equal(xSubstring("a", 1), "a")
	expect_equal(xSubstring("abc", 1:3), "abc")
	expect_equal(xSubstring("abcd", c(1, 3)), 'ac')

})
