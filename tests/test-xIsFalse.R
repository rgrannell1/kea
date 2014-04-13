
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xIsFalse")

	forall(
		"istrue of empty collection is False",
		test_cases$collection_zero,
		!xIsFalse(coll)
	)
