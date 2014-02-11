
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


inc <- function (a) a + 1

message("xFlip")

	forall(
		"flipping the null function returns the null function",
		list(),
		is.null(formals( xFlip( function () {} )) )
	)

	forall(
		"double-flipping is identity",
		test_cases$num_positive_integer,
		xFlip(xFlip(inc))(num) == inc(num)
	)

