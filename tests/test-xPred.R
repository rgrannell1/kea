
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xPred')

	forall(
		"the predeccesor of an empty collection is an empty double",
		test_cases$collection_zero,
		xPred(coll) %equals% as.double(coll)
	)

	forall(
		"the predeccesor is doubles",
		test_cases$integers,
		xPred(coll) %equals% as.double(coll - 1)
	)

	forall(
		"infinity acts as an identity",
		test_cases$infinity,
		xPred(coll) %equals% as.double(coll)
	)

message('arrow $ xPred')

	forall(
		"collection $ xPred",
		test_cases$infinity,
		x_(coll)$xPred()$x() %equals% as.double(coll)
	)

message('arrow $ xPred...')

message('arrow $ x_Pred')

	forall(
		"collection $ x_Pred",
		test_cases$infinity,
		x_(coll)$x_Pred() %equals% as.double(coll)
	)
