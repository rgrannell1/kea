
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xConcat")

	forall(
		"a single collection acts as identity",
		test_cases$collection,
		xConcat(list(coll)) %equals% list(coll)
	)

	forall(
		"a single collection and null acts as identity",
		test_cases$collection,
		xConcat(list(coll, Null)) %equals% list(coll)
	)

message("xConcat...")


message("arrow $ xConcat")





message("arrow $ xConcat...")


