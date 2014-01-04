
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xDuplicated')

	forall(
		"duplicate of empty coll is empty list",
		test_cases$collection_zero,
		xDuplicated(coll) %equals% list()
	)

	forall(
		"union and duplicate contain all elements ",
		test_cases$integers,
		length(coll) == length(unique(coll)) + length(xDuplicated(coll))
	)

message('arrow $ xDuplicated')

	forall(
		"coll $ xDuplicated",
		test_cases$integers,
		length(coll) == length(unique(coll)) + length( x_(coll)$xDuplicated()$x_() )
	)

	forall(
		"coll $ x_Duplicated",
		test_cases$integers,
		length(coll) == length(unique(coll)) + length( x_(coll)$x_Duplicated() )
	)

