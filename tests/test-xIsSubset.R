
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xIsSubset")

	forall(
		"elements of the collection is always a subset",
		test_cases$collection,
		xIsSubset(
			rsample(coll, size = rsample(1:length(coll), size = 1)),
			coll
		),
		given =
			length(coll) > 0
	)
