
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xIsNan")

	forall(
		"istrue of empty collection is False",
		test_cases$collection_zero,
		!xIsNan(coll)
	)

	forall(
		"isnan of nan is true",
		list(),
		xIsNan(NaN)
	)
