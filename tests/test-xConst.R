
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xConst")

	forall(
		"const of a value is a function",
		test_cases$collection,
		xConst(coll)() %equals% coll
	)

