
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xPack")

	forall(
		"packing the empty collection returns the empty list",
		test_cases$collection_zero,
		xPack(coll) %equals% list()
	)

	forall(
		"packing a list of empty lists returns the empty list",
		test_cases$collection_of_length_zero,
		xPack(coll) %equals% list()
	)

message("xPack")

	forall(
		"coll $ xPack",
		test_cases$collection_zero,
		x_(coll)$xPack()$x() %equals% list()
	)
