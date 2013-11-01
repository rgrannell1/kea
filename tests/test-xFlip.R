
message("xFlip")

test_that("xFlip", {

	expect_equal(
		xFormals(xFlip(function (a, b) {} )),
		alist(b=, a=))

	expect_equal(
		xFormals(xFlip(function (a, b, c=1) {} ))$c,
		1)

	expect_equal(
		xFlip(function (a, b) a + b)(1, 2),
		3)
})
