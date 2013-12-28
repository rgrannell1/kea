
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

message("arrow $ xJoin")

	forall(
		"a single collection and null acts as identity",
		test_cases$collection,
		x_( list(coll, Null) )$xJoin()$x() %equals% as.list(coll)
	)

message("arrow $ x_Concat")

	forall(
		"a single collection and null acts as identity",
		test_cases$collection,
		x_( list(coll, Null) )$x_Concat() %equals% as.list(coll)
	)
