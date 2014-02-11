
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message('xSuccOf')

	forall(
		"the successor of an empty collection is an empty double",
		test_cases$collection_zero,
		xSuccOf(coll) %equals% as.double(coll)
	)

	forall(
		"the successor is doubles",
		test_cases$integers,
		xSuccOf(coll) %equals% as.double(coll + 1)
	)

	forall(
		"infinity acts acts an identity",
		test_cases$infinity,
		xSuccOf(coll) %equals% as.double(coll)
	)

