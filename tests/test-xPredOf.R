
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xPredOf')

	forall(
		"the predeccesor of an empty collection is an empty double",
		test_cases$collection_zero,
		xPredOf(coll) %equals% as.double(coll)
	)

	forall(
		"the predeccesor is doubles",
		test_cases$integers,
		xPredOf(coll) %equals% as.double(coll - 1)
	)

	forall(
		"infinity acts as an identity",
		test_cases$infinity,
		xPredOf(coll) %equals% as.double(coll)
	)
