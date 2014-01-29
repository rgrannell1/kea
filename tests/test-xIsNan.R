
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xIsNan")

	forall(
		"istrue of empty collection is False",
		test_cases$collection_zero,
		!xIsNan(coll)
	)
