
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xRejectNull")

	forall(
		"removenull of the empty collection is list()",
		test_cases$collection_zero,
		xRejectNull(coll) %equals% list()
	)
