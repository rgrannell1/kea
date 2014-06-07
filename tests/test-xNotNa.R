
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xNotNa")

	forall(
		"not na of empty collection is False",
		test_cases$collection_zero,
		xNotNa(coll)
	)

	forall(
		"not na of na is false",
		list(),
		!xNotNa(Na)
	)
