
context('xSubString')

test_that('xSubString', {

	expect_error(xSubString("1", 100))
	expect_equal(xSubString('', 0), '')
	expect_equal(xSubString("a", 1), "a")
	expect_equal(xSubString("abc", 1:3), "abc")
	expect_equal(xSubString("abcd", c(1, 3)), 'ac')

})
