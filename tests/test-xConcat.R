
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xConcat")

	forall(
		"a single collection acts as identity",
		test_cases$collection,
		xConcat(list(coll)) %equals% as.list(coll)
	)

	forall(
		"a single collection and null acts as identity",
		test_cases$collection,
		xConcat(list(coll, Null)) %equals% as.list(coll)
	)

message("xConcat...")

message("arrow $ xConcat")

	forall(
		"a single collection and null acts as identity",
		test_cases$collection,
		x_( list(coll, Null) )$xConcat()$x() %equals% as.list(coll)
	)

message("arrow $ xConcat...")


