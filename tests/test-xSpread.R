
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xSpread")

	forall(
		"identity as variadic collects a list of arguments.",
		test_cases$num_integer,
		xSpread(identity)(num, num) %is% list(num, num)
	)
