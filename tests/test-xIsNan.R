
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xIsNan')

	forall(
		"a vector of nan's is nan",
		test_cases$nans,
		all(xIsNan(coll))
	)

message('arrow $ xIsNan')

	forall(
		"collection $ xIsNan",
		test_cases$nans,
		all(x_(coll)$xIsNan()$x())
	)

message('arrow $ x_IsNan')

	forall(
		"collection $ x_IsNan",
		test_cases$nans,
		all(x_(coll)$x_IsNan())
	)
