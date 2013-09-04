
context("xEqual")

test_that("xEqual", {

	expect_equal(xEqual(xK(1), xK(1))(1, 2, 3), True)
	expect_equal(xEqual(xK(1), xK(2))(1, 2, 3), False)
	expect_equal(
		xEqual(
			function (a, b) a+b, 
			function (a, b) a*b)(0, 0), True)
	expect_equal(
		xEqual(
			function (a, b) a+b, 
			function (a, b) a*b)(0, 10), False)

})
