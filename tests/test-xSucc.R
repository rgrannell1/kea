
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xSucc')

	forall(
		"the successor of an empty collection is an empty double",
		test_cases$collection_zero,
		xSucc(coll) %equals% as.double(coll)
	)

	forall(
		"the successor is doubles",
		test_cases$integers,
		xSucc(coll) %equals% as.double(coll + 1)
	)

	forall(
		"infinity acts acts an identity",
		test_cases$infinity,
		xSucc(coll) %equals% as.double(coll)
	)

message('arrow $ xSucc')

	forall(
		"collection $ xSucc",
		test_cases$infinity,
		x_(coll)$xSucc()$x_() %equals% as.double(coll)
	)

message('arrow $ xSucc...')

	forall(
		"collection $ xSucc...",
		test_cases$integers,
		x_(coll)$xApply(xSucc...)$x_() %equals% as.double(coll + 1)
	)

message('arrow $ x_Succ')

	forall(
		"collection $ x_Succ",
		test_cases$infinity,
		x_(coll)$x_Succ() %equals% as.double(coll)
	)
