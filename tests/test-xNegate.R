
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases
is_na <- kiwi:::is_na

require(kiwi)

message("xNegate")

	forall(
		"test that negate (pred) is opposite pred",
		test_cases$logical_function,
		!fn() %equals% xNegate(fn)(),
		given =
			!is_na(fn())
	)

	forall(
		"negated na is na",
		test_cases$logical_function,
		fn() %equals% xNegate(fn)(),
		given =
			is_na(fn())
	)

