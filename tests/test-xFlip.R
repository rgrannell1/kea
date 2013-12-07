
forall <- arrow:::forall
test_cases <- arrow:::test_cases

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

message("arrow $ xFlip")

	forall(
		"double-flipping is identity",
		test_cases$positive_with_linear_function,
		x_(fn)$xFlip()$xFlip()$x()(num) == fn(num)
	)