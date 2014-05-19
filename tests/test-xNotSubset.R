
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

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
