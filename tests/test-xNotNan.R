
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xNotNan")

	forall(
		"not false of empty collection is False",
		test_cases$collection_zero,
		xNotNan(coll)
	)

	forall(
		"isnan of nan is true",
		list(),
		!xNotNan(NaN)
	)
