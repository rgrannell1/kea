
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xNotSubset")

	forall(
		"elements of the collection is always a subset",
		test_cases$collection,
		!xNotSubset(
			rsample(as.list(coll), size = rsample(1:length(coll), size = 1)),
			coll
		),
		given =
			length(coll) > 0
	)
