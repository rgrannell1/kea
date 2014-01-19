
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xAsVariadic")

	forall(
		"identity as variadic collects a list of arguments.",
		test_cases$num_integer,
		xAsVariadic(identity)(num, num) %equals% list(num, num)
	)
