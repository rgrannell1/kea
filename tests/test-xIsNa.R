
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xIsNa")

	forall(
		"istrue of empty collection is False",
		test_cases$collection_zero,
		!xIsNa(coll)
	)

	forall(
		"is na of na is true",
		list(),
		xIsNa(Na)
	)
