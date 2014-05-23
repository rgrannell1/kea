
forall <- arrow:::forall
test_cases <- arrow:::test_cases
is_na <- arrow:::is_na

require(arrow)

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