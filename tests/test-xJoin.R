
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xJoin")

	forall(
		"a single collection acts as identity",
		test_cases$collection,
		xJoin(list(coll)) %equals% as.list(coll)
	)

	forall(
		"a single collection and null acts as identity",
		test_cases$collection,
		xJoin(list(coll, Null)) %equals% as.list(coll)
	)

message("xJoin...")
