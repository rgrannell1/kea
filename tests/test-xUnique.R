
message('xUnique')

	forall(
		"unique of empty coll is empty list",
		test_cases$collection_zero,
		xUnique(coll) %equals% list()
	)

	forall(
		"union and duplicate contain all elements ",
		test_cases$integers,
		length(coll) == length(xUnique(coll)) + length( which(duplicated(coll)) )
	)

message('arrow $ xUnique')

	forall(
		"arrow $ xUnique",
		test_cases$integers,
		length(coll) == length(x_(coll)$xUnique()$x_()) + length( which(duplicated(coll)) )
	)

	forall(
		"arrow $ x_Unique",
		test_cases$integers,
		length(coll) == length(x_(coll)$x_Unique()) + length( which(duplicated(coll)) )
	)
