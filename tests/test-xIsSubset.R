
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xIsSubset")

	forall(
		"elements of the collection is always a subset",
		test_cases$collection,
		xIsSubset(
			sample(coll, size = sample(1:length(coll), size = 1)),
			coll
		),
		given =
			length(coll) > 0
	)
