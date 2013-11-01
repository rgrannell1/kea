
message("xEqualLift")

test_that("xEqualLift", {

	expect_equal(xEqualLift(xK(1), xK(1))(1, 2, 3), True)
	expect_equal(xEqualLift(xK(1), xK(2))(1, 2, 3), False)
	expect_equal(
		xEqualLift(
			function (a, b) a+b,
			function (a, b) a*b)(0, 0), True)
	expect_equal(
		xEqualLift(
			function (a, b) a+b,
			function (a, b) a*b)(0, 10), False)

})
