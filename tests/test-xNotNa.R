
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


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
