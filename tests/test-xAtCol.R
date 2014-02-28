
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xAtCol")

	forall(
		"selecting the empty list is the empty list",
		test_cases$positive_with_collection_zero,
		xAtCol(num, list()) %equals% list()
	)
