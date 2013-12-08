
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xConst")

	forall(
		"const of a value is a function",
		test_cases$collection,
		xConst(coll)() %equals% coll
	)



message("arrow $ xConst")

	forall(
		"const of a value is a function",
		test_cases$collection,
		x_(coll)$xConst()$x()() %equals% coll
	)
