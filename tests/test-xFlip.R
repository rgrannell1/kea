
message("xFlip")

forall(
	"flipping the null function returns the null function",
	list(),
	is.null(formals( xFlip( function () {} )) )
)

forall(
	"double-flipping is identity",
	test_cases$positive_with_linear_function,
	xFlip(xFlip(fn))(num) == fn(num)
)







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
