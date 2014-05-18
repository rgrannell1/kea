
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xPowerSetOf")

	forall(
		"power set of empty set is list()",
		test_cases$collection_zero,
		xPowerSetOf(coll) %equals% list()
	)

	forall(
		"power set has correct length",
		test_cases$num_positive_integer,
		length(xPowerSetOf(1:num)) == 2^num,
		given =
			num < 10
	)
