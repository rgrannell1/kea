
context('xSubstr')

test_that('xSubstr', {

	expect_error(xSubstr("1", 100))
	expect_equal(xSubstr('', 0), '')
	expect_equal(xSubstr("a", 1), "a")
	expect_equal(xSubstr("abc", 1:3), "abc")
	expect_equal(xSubstr("abcd", c(1, 3)), 'ac')

})
