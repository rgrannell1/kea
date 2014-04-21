
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xSpread")

	forall(
		"identity as variadic collects a list of arguments.",
		test_cases$num_integer,
		xSpread(identity)(num, num) %equals% list(num, num)
	)
