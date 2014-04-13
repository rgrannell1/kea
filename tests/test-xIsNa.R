
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

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