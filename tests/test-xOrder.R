
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xOrder")

	forall(
		"order of empty collection is integer(0)",
		test_cases$collection_zero,
		xOrder(coll) %equals% integer(0)
	)