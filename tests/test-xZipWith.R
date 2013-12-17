
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xZipWith")

	forall(
		"zipwithing the empty collection is the empty list",
		test_cases$collection_zero,
		xZipWith(identity, coll) %equals% list()
	)



message("arrow $ xZipWith")

