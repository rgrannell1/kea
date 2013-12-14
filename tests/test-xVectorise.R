
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message("xVectorise")

	forall(
		"partmapping over an empty list is the empty list",
		test_cases$collection,
		xVectorise(function (x) x^2)(coll) %equals% list(),
		given =
			length(coll) == 0
	)

	forall(
		"partmapping identity across a collection is the list identity",
		test_cases$collection,
		xVectorise(identity)(coll) %equals% as.list(coll),
		given =
			length(coll) > 0
	)

	forall(
		"part-mapping increment over integers works",
		test_cases$succ_over_integers,
		all( xVectorise(fn)(coll) == unlist(coll) + 1 )
	)

message("arrow $ xVectorise")

	forall(
		"function $ xVectorise increments over integers",
		test_cases$succ_over_integers,
		all( (x_(fn)$xVectorise()$x())(coll) == unlist(coll) + 1 )
	)
