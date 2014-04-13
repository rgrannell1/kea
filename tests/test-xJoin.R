
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)

message("xJoin")

	forall(
		"a single collection acts as identity",
		test_cases$collection,
		xJoin(list(coll)) %equals% as.list(coll)
	)

	forall(
		"a single collection and null acts as identity",
		test_cases$collection_and_collection_zero,
		xJoin(list(coll2, coll1)) %equals% xJoin(list(coll1, coll2))
	)
