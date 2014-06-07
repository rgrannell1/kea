
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xAtCol")

	forall(
		"selecting the empty list is the empty list",
		test_cases$positive_with_collection_zero,
		xAtCol(num, list()) %equals% list()
	)
