
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xIsSubset")

	# -- note; as list is needed to avoid the crappy, crappy length-one
	# -- corner case of sample(); sample(10, size = 1) != 10

	forall(
		"elements of the collection is always a subset",
		test_cases$collection,
		xIsSubset(
			sample(as.list(coll), size = sample(1:length(coll), size = 1)),
			coll
		),
		given =
			length(coll) > 0
	)
