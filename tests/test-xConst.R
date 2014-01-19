
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xConst")

	forall(
		"const of a value is a function",
		test_cases$collection,
		xConst(coll)() %equals% coll
	)

	forall(
		"const can swallow as many arguments as its given.",
		test_cases$collection,
		do.call( xConst(1), as.list(coll) ) == 1
	)
