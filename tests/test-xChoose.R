
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xChoose")

	forall(
		"combos of empty collection is empty list",
		test_cases$nonnegative_with_collection_zero,
		xChoose(num, coll) %equals% list()
	)

