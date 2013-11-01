
message('xSubStr')

test_that('xSubStr', {

	expect_error(xSubStr("1", 100))
	expect_equal(xSubStr('', 0), '')
	expect_equal(xSubStr("a", 1), "a")
	expect_equal(xSubStr("abc", 1:3), "abc")
	expect_equal(xSubStr("abcd", c(1, 3)), 'ac')

})
