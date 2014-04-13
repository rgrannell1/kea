
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xNotTrue")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotTrue(coll)
	)

	forall(
		"not True of True is false",
		list(),
		!xNotTrue(True)
	)
