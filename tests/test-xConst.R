
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
		"function $ xConst",
		test_cases$collection,
		x_(coll)$xConst()$x()() %equals% coll
	)

message("arrow $ x_Const")

	forall(
		"function $ x_Const",
		test_cases$collection,
		x_(coll)$x_Const()() %equals% coll
	)
